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

package scala.reflect.runtime

import org.junit.Assert._
import org.junit.Test

/** A symbol of runtime reflection that reports itself threadsafe is read without the GIL,
 *  so it must not still have a completer for its info.
 */
class CompletionMarkingTest {
  @Test def unpickledSymbolsAreNotThreadsafeBeforeTheirInfoIsComplete(): Unit = {
    val u = new JavaUniverse
    import u._
    val mirror = u.runtimeMirror(getClass.getClassLoader)
    def check(sym: Symbol): Unit =
      assertFalse(s"$sym is marked threadsafe with info ${sym.rawInfo}", sym.isThreadsafe(AllOps) && sym.rawInfo.isInstanceOf[LazyType])
    for (name <- List("scala.io.BufferedSource", "scala.collection.immutable.ListMap")) {
      val clazz = mirror.staticClass(name)
      val module = clazz.companionModule
      val related = List(clazz, module, module.moduleClass).filter(_ ne NoSymbol)
      // Run only the top-level completer: it unpickles the class and its companion and leaves
      // (at least) the class's info as the unpickler's lazy type.
      clazz.rawInfo.load(clazz)
      assertTrue(s"${clazz.rawInfo}", clazz.rawInfo.isInstanceOf[LazyType])
      related.foreach(check)
      // Completing one of them must not mark the others while their infos are still lazy.
      for (sym <- related) { sym.info; related.foreach(check) }
      for (sym <- related) assertTrue(s"$sym is not marked threadsafe once complete", sym.isThreadsafe(AllOps))
    }
  }
}
