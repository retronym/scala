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

package scala.reflect.macros
package runtime

import scala.reflect.runtime.ReflectionUtils
import scala.reflect.macros.blackbox.{Context => BlackboxContext}
import scala.reflect.macros.whitebox.{Context => WhiteboxContext}
import java.lang.reflect.{Constructor => jConstructor}

trait JavaReflectionRuntimes {
  self: scala.tools.nsc.typechecker.Analyzer =>

  trait JavaReflectionResolvers {
    self: MacroRuntimeResolver =>

    def resolveJavaReflectionRuntime(classLoader: ClassLoader): MacroRuntime = {
      val implClass = Class.forName(className, true, classLoader)
      val implMeths = implClass.getMethods.find(_.getName == methName)
      // relies on the fact that macro impls cannot be overloaded
      // so every methName can resolve to at maximum one method
      val implMeth = implMeths getOrElse { throw new NoSuchMethodException(s"$className.$methName") }
      macroLogVerbose(s"successfully loaded macro impl as ($implClass, $implMeth)")
      if (isBundle) {
        val isRepl = implClass.getEnclosingClass.getSimpleName == "$iw"

        def isMacroContext(clazz: Class[_]) = clazz == classOf[BlackboxContext] || clazz == classOf[WhiteboxContext]

        def isBundleCtor(ctor: jConstructor[_]) = ctor.getParameterTypes match {
          case Array(param) if isMacroContext(param) => true
          case Array(outer, param) if isRepl && isMacroContext(param) => true
          case _ => false
        }

        val Array(bundleCtor) = implClass.getConstructors.filter(isBundleCtor)
        val iwInstance = ReflectionUtils.interpreterInstance(implClass.getEnclosingClass)
        args => {
          val bundleArgs = if (isRepl && bundleCtor.getParameterCount == 2) {
            Array(iwInstance, args.c)
          }
          else Array(args.c)
          val implObj = bundleCtor.newInstance(bundleArgs: _*)
          implMeth.invoke(implObj, args.others.asInstanceOf[Seq[AnyRef]]: _*)
        }
      } else {
        val isRepl = implClass.getSimpleName == "$iw"

        val implObj = if (isRepl) {
          // Special case to let for REPL -Yclass-based users define macro implementations.
          // Reflectively call `$read$.MODULE$.$iw().$iw().$iw()` to get the `this` of the macro implementation.
          val loader = implClass.getClassLoader
          var readClass = implClass.getEnclosingClass
          while (readClass.getEnclosingClass != null) {
            readClass = readClass.getEnclosingClass
          }
          val read = ReflectionUtils.staticSingletonInstance(loader, readClass.getName)
          val instanceMethod = read.getClass.getMethod("INSTANCE")
          val instance = instanceMethod.invoke(read)
          val iwName = implClass.getSimpleName
          var outer = instance
          var iwInstance = ReflectionUtils.innerSingletonInstance(instance, iwName)
          while (iwInstance.getClass != implClass) {
            outer = iwInstance
            iwInstance = ReflectionUtils.innerSingletonInstance(outer, iwName)
          }
          iwInstance
        } else
          ReflectionUtils.staticSingletonInstance(implClass)
        args => {
          val implArgs = args.c +: args.others
          implMeth.invoke(implObj, implArgs.asInstanceOf[Seq[AnyRef]]: _*)
        }
      }
    }
  }
}