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

package scala.tools.nsc.transform.async

import scala.collection.mutable.ListBuffer
import scala.tools.nsc.Mode
import scala.tools.nsc.transform.async.user.{FutureSystem, ScalaConcurrentFutureSystem}
import scala.tools.nsc.transform.{Transform, TypingTransformers}

abstract class AsyncPhase extends Transform with TypingTransformers with AsyncTransform{
  self =>
  import global._

  val asyncNames = new AsyncNames[global.type](global)
  val tracing = new Tracing

  val phaseName: String = "async"
  override def enabled = true // TODO: should be off by default, enabled by flag
//  {
//    (currentRun.runDefinitions match {
//      case null => new definitions.RunDefinitions
//      case rd => rd
//    }).Async_async.exists
//  }
  final class FutureSystemAttachment(val system: FutureSystem) extends PlainAttachment

  val futureSystemsCache = perRunCaches.newMap[String, FutureSystem]
  val earlyExpansionCache = perRunCaches.newMap[String, AsyncEarlyExpansion { val global: self.global.type }]

  import treeInfo.Applied

  def fastTrackImpl(app: Applied, resultTp: Tree, asyncBody: Tree, execContext: Option[Tree]): scala.reflect.macros.contexts.Context { val universe: self.global.type } => Tree = {
    c => {
      val fsname = app.tree.symbol.getAnnotation(c.global.currentRun.runDefinitions.Async_asyncMethod).flatMap(_.stringArg(0)).get
      val exp = c.global.async.earlyExpansionCache.getOrElseUpdate(fsname, new AsyncEarlyExpansion {
        val global: self.global.type = self.global
        //val u: c.universe.type = c.universe
        val futureSystem = futureSystemsCache.getOrElseUpdate(fsname, {
          val clazz = Thread.currentThread().getContextClassLoader.loadClass(fsname+"$") // TODO: Is this the right way to load a class?
          clazz.getField("MODULE$").get(clazz).asInstanceOf[FutureSystem]
        })
      })
      exp(c.callsiteTyper, asyncBody, execContext, resultTp.tpe, c.internal.enclosingOwner)
    }
  }

  def fastTrackAnnotationEntry: (Symbol, PartialFunction[Applied, scala.reflect.macros.contexts.Context { val universe: self.global.type } => Tree]) =
    (currentRun.runDefinitions.Async_asyncMethod, {
      // def async[T](body: T)(implicit execContext: ExecutionContext): Future[T] = macro ???
      case app@Applied(_, resultTp :: Nil, List(asyncBody :: Nil, execContext :: Nil)) =>
        fastTrackImpl(app, resultTp, asyncBody, Some(execContext))
      case app@Applied(_, resultTp :: Nil, (asyncBody :: Nil) :: Nil) =>
        fastTrackImpl(app, resultTp, asyncBody, None)
    })

  def newTransformer(unit: CompilationUnit): Transformer = new AsyncTransformer(unit)

  // TODO: support more than the original late expansion tests
  // TOOD: figure out how to make the root-level async built-in macro sufficiently configurable:
  //       replace the ExecutionContext implicit arg with an AsyncContext implicit that also specifies the type of the Future/Awaitable/Node/...?
  class AsyncTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    lazy val uncurrier = new uncurry.UnCurryTransformer(unit)
    lazy val eraser = new erasure.ErasureTransformer(unit)

    override def transform(tree: Tree): Tree =
      super.transform(tree) match {
        // {
        //    class stateMachine$async extends scala.runtime.AbstractFunction1 with Function0$mcV$sp {
        //      def apply(tr$async: scala.util.Try): Unit = { // symbol of this def is `applySym`, symbol of its param named "tr$async" is `trParamSym`
        //      ...
        //    }
        //    val stateMachine = ...
        //    ...
        // }
        case tree: Block if tree.hasAttachment[FutureSystemAttachment] =>
          val saved = currentTransformState
          val futureSystem = tree.getAndRemoveAttachment[FutureSystemAttachment].get.system
          val newState = new AsyncTransformState[global.type](global, futureSystem, unit, this)
          currentTransformState = newState
          try tree.stats match {
            case List(
                temp@ValDef(_, nme.execContextTemp, _, execContext),
                cd@ClassDef(mods, tpnme.stateMachine, _, impl@Template(parents, self, stats)),
                vd@ValDef(_, nme.stateMachine, tpt, _),
                rest @ _*) if tpt.tpe.typeSymbol == cd.symbol =>
              val ((dd: DefDef) :: Nil, others) = stats.partition {
                case dd@DefDef(mods, nme.apply, _, List(tr :: Nil), _, _) => !dd.symbol.isBridge
                case _ => false
              }
              val asyncBody = (dd.rhs: @unchecked) match {
                case blk@Block(stats, Literal(Constant(()))) => treeCopy.Block(blk, stats.init, stats.last)
              }
              val (newRhs, liftables) = asyncTransform(asyncBody, dd.symbol, dd.vparamss.head.head.symbol)

              val newApply = deriveDefDef(dd)(_ => newRhs).setType(null) /* need to retype */
              val newStats = new ListBuffer[Tree]
              newStats ++= others
              newStats += newApply
              newStats ++= liftables
              val newTempl = treeCopy.Template(impl, parents, self, newStats.toList)
              val ucd = treeCopy.ClassDef(cd, mods, tpnme.stateMachine, Nil, newTempl)
              treeCopy.Block(tree, temp :: localTyper.typedClassDef(ucd) :: vd :: rest.toList, tree.expr)
          } finally {
            currentTransformState = saved
          }
        case tree => tree
      }
  }
}
