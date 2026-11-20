import java.nio.file.Files

import scala.tools.partest.StoreReporterDirectTest

// A file that is not a class file is reported as an error at its first use, it does not crash the compiler.
object Test extends StoreReporterDirectTest {
  def code = "class Bar extends Foo"

  def bytes(bs: Int*): Array[Byte] = bs.map(_.toByte).toArray
  val magic = List(0xCA, 0xFE, 0xBA, 0xBE)

  val broken = List(
    "truncated"       -> bytes(magic ++ List(0, 0, 0, 52, 0): _*),
    "wrong magic"     -> "not a class file".getBytes("UTF-8"),
    // JDK 1.0.2 wrote 45.3, nothing older is supported
    "old version"     -> bytes(magic ++ List(0, 0, 0, 44) ++ List.fill(16)(0): _*),
    // the constant pool has one entry with the unknown tag 99
    "bad pool tag"    -> bytes(magic ++ List(0, 0, 0, 52, 0, 2, 99) ++ List.fill(16)(0): _*),
    // the constant pool is empty, the index 0 of the class name is not an entry
    "bad pool entry"  -> bytes(magic ++ List(0, 0, 0, 52, 0, 1, 0, 0x21, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0): _*)
  )

  override def extraSettings = s"-usejavacp -cp ${testOutput.path}"

  override def show(): Unit =
    for ((what, content) <- broken) {
      Files.write(testOutput.jfile.toPath.resolve("Foo.class"), content)
      println(s"$what:")
      assert(!compile())
      // the detail of an exception depends on the JDK
      val msg = storeReporter.infos.head.msg.replace(testOutput.path, "<out>").replaceAll("""\(class [\w.]+/.*\)""", "(<exception>)")
      println("  " + msg.replace("\n", "\n  "))
    }
}
