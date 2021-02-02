import scala.reflect.runtime.currentMirror
import com.google.protobuf.DescriptorProtos.FileDescriptorProto

import scala.tools.nsc.util

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

package p {
  object Hello {
    def fromJava(t: FileDescriptorProto): Unit = ()
  }
}

object Test{
  def main(args: Array[String]): Unit = {
    def f() =
      try currentMirror.reflectModule(currentMirror.staticModule("p.Hello$")).instance
      catch { case t: Throwable => s"ugh, ${util.stackTraceString(t)}" }

    println(f())
    println(f())
  }
}
