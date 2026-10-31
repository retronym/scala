/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.jpms

import java.lang.module.{ModuleDescriptor, ModuleFinder}
import java.net.URI
import java.nio.file.{FileSystems, Files, Path, Paths}

import scala.jdk.CollectionConverters._
import scala.reflect.io.PlainNioFile
import scala.tools.nsc.io.AbstractFile
import scala.util.Using

/** The system modules when they are not those of the running JDK: for `-release` and `-system`. */
private[nsc] object SystemModules {
  /** The modules of the JDK at `javaHome`, as `-system` names it, read from its `jrt:` file system. */
  def ofJdk(javaHome: String): ModuleFinder = {
    val fs = FileSystems.newFileSystem(URI.create("jrt:/"), Map("java.home" -> javaHome).asJava)
    val modules = Files.list(fs.getPath("/modules")).iterator.asScala.toList.flatMap { dir =>
      val moduleInfo = dir.resolve("module-info.class")
      if (Files.isRegularFile(moduleInfo)) Using.resource(Files.newInputStream(moduleInfo))(in => List(ModuleDescriptor.read(in)))
      else Nil
    }
    FixedModuleFinder.newModuleFinder(modules.asJava)
  }

  /** The modules of Java SE `release` (9 or later), as the running JDK's `ct.sym` describes them (JEP 247).
   *
   *  `ct.sym` has a directory per set of releases that share a version of a file, named by the releases' codes
   *  (`9`, `A` for 10, `B` for 11, ...). Below that is a directory per module, holding its `module-info.sig` and
   *  its packages' `.sig` files. A module's packages for a release may be spread over several of these.
   */
  def ofRelease(release: Int): ModuleFinder = {
    val ctSym = Paths.get(System.getProperty("java.home"), "lib", "ct.sym")
    val fs = FileSystems.newFileSystem(ctSym, null: ClassLoader)
    val code = if (release < 10) release.toString else ('A' + (release - 10)).toChar.toString
    val roots = Files.list(fs.getRootDirectories.iterator.next).iterator.asScala.toList.filter { root =>
      val name = root.getFileName.toString.stripSuffix("/")
      !name.contains("-") && name.contains(code)
    }
    val moduleDirs: Map[String, List[Path]] =
      roots.flatMap(root => Files.list(root).iterator.asScala.filter(Files.isDirectory(_)).toList)
        .groupBy(_.getFileName.toString.stripSuffix("/"))
    val modules = moduleDirs.toList.flatMap { case (_, dirs) =>
      dirs.map(_.resolve("module-info.sig")).find(Files.isRegularFile(_)).map { moduleInfo =>
        val packages = dirs.flatMap { dir =>
          Files.walk(dir).iterator.asScala.filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".sig") && p.getParent != dir)
            .map(p => dir.relativize(p.getParent).toString.stripSuffix("/").replace('/', '.'))
        }.toSet
        val bytes = Files.readAllBytes(moduleInfo)
        ModuleDescriptor.read(java.nio.ByteBuffer.wrap(bytes), () => (packages ++ packagesNamedIn(bytes)).asJava)
      }
    }
    val names = modules.map(_.name).toSet
    FixedModuleFinder.newModuleFinder(modules.map(forCompilation(_, names)).asJava)
  }

  /** `ct.sym` leaves out JDK-internal modules, such as `jdk.internal.jvmstat`, which some of its modules require, and
   *  the packages of service types and providers outside the API. Neither matters for compiling against the release,
   *  but resolution rejects them. So drop dependences on missing modules, and the `uses` and `provides`, which only
   *  matter for service binding at run time.
   */
  private def forCompilation(d: ModuleDescriptor, observable: Set[String]): ModuleDescriptor = {
    val b = ModuleDescriptor.newModule(d.name, d.modifiers)
    d.requires.asScala.filter(r => observable(r.name)).foreach(b.requires)
    d.exports.asScala.foreach(b.exports)
    d.opens.asScala.foreach(b.opens)
    b.packages(d.packages)
    d.rawVersion.ifPresent(v => b.version(v))
    d.mainClass.ifPresent(c => b.mainClass(c))
    b.build()
  }

  /** The packages a `module-info` class file names. `ct.sym` only has `.sig` files for API packages, but a module's
   *  qualified exports, opens and service providers may be in others, and the descriptor is only valid if its
   *  packages include them.
   */
  private def packagesNamedIn(moduleInfo: Array[Byte]): Set[String] = {
    import scala.tools.asm.{ClassReader, ClassVisitor, ModuleVisitor, Opcodes}
    val result = Set.newBuilder[String]
    def pkg(internalName: String) = internalName.replace('/', '.')
    def pkgOfClass(internalName: String) = internalName.lastIndexOf('/') match { case -1 => "" case i => pkg(internalName.substring(0, i)) }
    new ClassReader(moduleInfo).accept(new ClassVisitor(Opcodes.ASM9) {
      override def visitModule(name: String, access: Int, version: String): ModuleVisitor = new ModuleVisitor(Opcodes.ASM9) {
        override def visitPackage(packaze: String): Unit = result += pkg(packaze)
        override def visitExport(packaze: String, access: Int, modules: String*): Unit = result += pkg(packaze)
        override def visitOpen(packaze: String, access: Int, modules: String*): Unit = result += pkg(packaze)
        override def visitProvide(service: String, providers: String*): Unit = providers.foreach(p => result += pkgOfClass(p))
      }
    }, 0)
    result.result()
  }

  /** The module of a class in the system image or `ct.sym`, from its path: `/modules/<module>/...` in a `jrt:`
   *  file system, `/<release codes>/<module>/...` in `ct.sym`.
   */
  def moduleOf(file: AbstractFile): String = file match {
    case f: PlainNioFile if f.nioPath.getNameCount > 2 => f.nioPath.getName(1).toString.stripSuffix("/")
    case _                                             => ""
  }
}
