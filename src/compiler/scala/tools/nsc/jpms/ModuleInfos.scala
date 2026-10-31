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

import java.lang.module.ModuleDescriptor
import java.lang.module.ModuleDescriptor.{Exports, Opens, Requires}

import scala.jdk.CollectionConverters._
import scala.tools.asm.{ClassWriter, Opcodes}
import scala.tools.nsc.Global
import scala.util.control.NonFatal

/** Module descriptors for a `module-info.java` compiled by scalac, and the `module-info.class` written for it. */
final class ModuleInfos[G <: Global](val global: G) {
  import global._

  /** The descriptor used to resolve the module graph: the module's dependences, exports and opens.
   *  `requires static` is treated as mandatory, as javac does at compile time.
   */
  def resolutionDescriptor(md: Jpms.ModuleDef): Option[ModuleDescriptor] =
    build(md, forResolution = true, imports = Nil)

  /** The full descriptor, with the service types and providers of `uses` and `provides` resolved to binary
   *  names. Must be called after typer, as it looks up classes in the symbol table.
   */
  def fullDescriptor(md: Jpms.ModuleDef, imports: List[Import]): Option[ModuleDescriptor] =
    build(md, forResolution = false, imports)

  private def build(md: Jpms.ModuleDef, forResolution: Boolean, imports: List[Import]): Option[ModuleDescriptor] = {
    val modifiers = if (md.open) Set(ModuleDescriptor.Modifier.OPEN) else Set.empty[ModuleDescriptor.Modifier]
    try {
      val builder = ModuleDescriptor.newModule(md.name.toString, modifiers.asJava)
      var ok = true
      md.directives.foreach {
        case Jpms.RequiresDirective(module, transitive, isStatic) =>
          val mods = Set.newBuilder[Requires.Modifier]
          if (transitive) mods += Requires.Modifier.TRANSITIVE
          if (isStatic && !forResolution) mods += Requires.Modifier.STATIC
          builder.requires(mods.result().asJava, module)
        case Jpms.ExportsDirective(pkg, Nil) => builder.exports(pkg)
        case Jpms.ExportsDirective(pkg, to)  => builder.exports(Set.empty[Exports.Modifier].asJava, pkg, to.toSet.asJava)
        case Jpms.OpensDirective(pkg, Nil)   => builder.opens(pkg)
        case Jpms.OpensDirective(pkg, to)    => builder.opens(Set.empty[Opens.Modifier].asJava, pkg, to.toSet.asJava)
        case d @ Jpms.UsesDirective(tpe) if !forResolution =>
          resolveType(tpe, imports, d.pos) match {
            case Some(service) => builder.uses(service)
            case None          => ok = false
          }
        case d @ Jpms.ProvidesDirective(tpe, providers) if !forResolution =>
          val service = resolveType(tpe, imports, d.pos)
          val impls = providers.map(resolveType(_, imports, d.pos))
          if (service.isEmpty || impls.exists(_.isEmpty)) ok = false
          else builder.provides(service.get, impls.flatten.asJava)
        case _ =>
      }
      if (ok) Some(builder.build()) else None
    } catch {
      // The builder validates names, and rejects duplicate directives, a module requiring itself, and the like.
      case ex @ (_: IllegalArgumentException | _: IllegalStateException) =>
        if (!forResolution) reporter.error(md.pos, s"invalid module declaration: ${ex.getMessage}")
        None
    }
  }

  /** Resolve a type name in a `uses` or `provides` directive to a binary class name, taking into account the
   *  imports of the `module-info.java` (JLS 7.7.1).
   */
  private def resolveType(name: String, imports: List[Import], pos: Position): Option[String] = {
    def qualifier(imp: Import) = imp.expr.toString.stripPrefix("_root_.").stripPrefix("<root>.")
    val (first, rest) = name.indexOf('.') match {
      case -1 => (name, "")
      case i  => (name.substring(0, i), name.substring(i))
    }
    val singleType = imports.collectFirst {
      case imp @ Import(_, List(sel)) if !sel.isWildcard && sel.name.toString == first => qualifier(imp) + "." + first + rest
    }
    val onDemand = imports.collect { case imp @ Import(_, List(sel)) if sel.isWildcard => qualifier(imp) + "." + name }
    val candidates = singleType.toList ::: name :: onDemand
    candidates.iterator.map(n => try rootMirror.getClassIfDefined(n) catch { case NonFatal(_) => NoSymbol }).find(_.exists) match {
      case Some(cls) => Some(cls.javaBinaryNameString.replace('/', '.')) // binary name (JLS 13.1), not the internal name
      case None      =>
        reporter.error(pos, s"type $name not found in module declaration")
        None
    }
  }

  /** The bytes of `module-info.class` for `descriptor`.
   *
   *  No `ModulePackages` attribute is written. It is optional, and a correct one needs the packages of every class
   *  in the module, which a separate or incremental compilation does not see. javac doesn't write it either; the
   *  `jar` tool adds it when packaging.
   */
  def classfileBytes(descriptor: ModuleDescriptor, classfileVersion: Int): Array[Byte] = {
    def internal(pkg: String) = pkg.replace('.', '/')
    def targets(ts: java.util.Set[String]): Array[String] = ts.asScala.toArray.sorted

    val cw = new ClassWriter(0)
    cw.visit(math.max(classfileVersion, Opcodes.V9), Opcodes.ACC_MODULE, "module-info", null, null, null)
    val moduleFlags = if (descriptor.isOpen) Opcodes.ACC_OPEN else 0
    val mv = cw.visitModule(descriptor.name, moduleFlags, descriptor.rawVersion.orElse(null))
    for (r <- descriptor.requires.asScala.toList.sortBy(_.name)) {
      var flags = 0
      r.modifiers.asScala.foreach {
        case Requires.Modifier.TRANSITIVE => flags |= Opcodes.ACC_TRANSITIVE
        case Requires.Modifier.STATIC     => flags |= Opcodes.ACC_STATIC_PHASE
        case Requires.Modifier.MANDATED   => flags |= Opcodes.ACC_MANDATED
        case Requires.Modifier.SYNTHETIC  => flags |= Opcodes.ACC_SYNTHETIC
      }
      mv.visitRequire(r.name, flags, r.rawCompiledVersion.orElse(null))
    }
    for (e <- descriptor.exports.asScala.toList.sortBy(_.source)) mv.visitExport(internal(e.source), 0, targets(e.targets): _*)
    for (o <- descriptor.opens.asScala.toList.sortBy(_.source)) mv.visitOpen(internal(o.source), 0, targets(o.targets): _*)
    for (u <- descriptor.uses.asScala.toList.sorted) mv.visitUse(internal(u))
    for (p <- descriptor.provides.asScala.toList.sortBy(_.service))
      mv.visitProvide(internal(p.service), p.providers.asScala.map(internal).toArray: _*)
    mv.visitEnd()
    cw.visitEnd()
    cw.toByteArray
  }
}
