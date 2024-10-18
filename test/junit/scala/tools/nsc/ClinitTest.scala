/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc

import java.io.{File, PrintWriter, StringWriter}
import scala.annotation.nowarn
import scala.collection.immutable.{List, Nil, Seq}
import scala.reflect.internal.Flags.{FINAL, STATIC}
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.tools.nsc.backend.jvm.AsmUtils
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.transform.TypingTransformers

object ClinitTest {

  def main(args: Array[String]): Unit = {
    val out = createTempDir()
    val settings = new scala.tools.nsc.Settings()
    settings.debug.value = true
    settings.outdir.value = out.getAbsolutePath
    settings.embeddedDefaults(getClass.getClassLoader)
    val isInSBT = !settings.classpath.isSetByUser
    if (isInSBT) settings.usejavacp.value = true

    val global = new Global(settings) {
      self =>
      @nowarn("cat=deprecation&msg=early initializers")
      object late extends {
        val global: self.type = self
      } with DemoPlugin

      override protected def loadPlugins(): List[Plugin] = late :: Nil
    }
    import global._
    val run = new Run()
    run.compileUnits(newCompilationUnit("class Staticify { def direct: String = null }") :: Nil)
    val loader = new URLClassLoader(Seq(new File(settings.outdir.value).toURI.toURL), global.getClass.getClassLoader)

    val bytecode = out.listFiles().flatMap { file =>
      val asmp = AsmUtils.textify(AsmUtils.readClass(file.getAbsolutePath))
      val sw = new StringWriter()

      asmp :: sw.toString :: Nil
    }.mkString("\n\n")
    println(bytecode)

    Class.forName("Staticify", true, loader)
  }

  private def createTempDir(): File = {
    val f = File.createTempFile("output", "")
    f.delete()
    f.mkdirs()
    f
  }
}
abstract class DemoPlugin extends Plugin {

  import global._
  override val description: String = "demo"
  override val name: String = "demo"

  override val components: List[PluginComponent] = List(new PluginComponent with TypingTransformers {
    val global: DemoPlugin.this.global.type = DemoPlugin.this.global
    override def newPhase(prev: Phase): Phase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        newTransformer(unit).transformUnit(unit)
      }
    }

    // If we run this before erasure we get an assertion error in specialConstructorErasure which expects
    // constructors to return the class type, but we're returning Unit.
    override val runsAfter: List[String] = "typer" :: Nil
    override val phaseName: String = "demo"
    private lazy val VarHandleClass = rootMirror.getClassIfDefined("java.lang.invoke.VarHandle")
    private lazy val MethodHandlesClass = rootMirror.getModuleIfDefined("java.lang.invoke.MethodHandles")

    def newTransformer(unit: CompilationUnit) = new ThicketTransformer(newRootLocalTyper(unit)) {
      override def transform(tree: Tree): Tree = tree match {

        case dd: DefDef if dd.name.string_==("direct") =>
          val cls = tree.symbol.owner
          assert(cls.isClass, cls)
          val implField = cls.newValue(dd.name.append("$impl").toTermName, tree.pos).setInfo(definitions.StringTpe)
          cls.info.decls.enter(implField)
          val implInit = q"null"
          val vhField = cls.newValue(dd.name.append("$vh").toTermName, tree.pos, newFlags = STATIC).setInfo(VarHandleClass.tpeHK)
          cls.info.decls.enter(vhField)
          val vhInit = q"$MethodHandlesClass.lookup().findVarHandle(classOf[$cls], ${implField.name.dropLocal.encoded}, classOf[${implField.info.resultType.typeSymbol}])"

          localTyper.typed(Thicket(Block(dd, localTyper.atOwner(cls).typed(newValDef(implField, implInit)()), localTyper.atOwner(cls).typed(newValDef(vhField, vhInit)()))))

        case _ =>
          super.transform(tree)
      }
    }
  })
}
