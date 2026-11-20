//> using jvm 9+

// `invoke` and a VarHandle access mode adapt the call site's descriptor with `asType`, so a call
// that returns Object followed by a cast is correct for them: this test runs and prints the
// expected values. Only `invokeExact` (and a VarHandle with invoke-exact behaviour) needs the
// result type set by ascription. The warnings in the .check are pinned as they are on 2.13.x.
//
// TODO scala/scala review Y3: if the warning is restricted to `invokeExact`, both warnings
// disappear from the .check (the output lines stay); if it gets its own -Wconf category, the
// category name appears in the .check instead.

import java.lang.invoke.{MethodHandles, MethodType}

object Test {
  def main(args: Array[String]): Unit = {
    val mh = MethodHandles.lookup().findStatic(classOf[String], "valueOf", MethodType.methodType(classOf[String], classOf[Object]))
    val c: AnyRef = Integer.valueOf(1)
    println(mh.invoke(c).asInstanceOf[String])

    val vh = MethodHandles.arrayElementVarHandle(classOf[Array[Long]])
    val arr = Array(42L)
    println(vh.getVolatile(arr, 0).asInstanceOf[Long])
  }
}
