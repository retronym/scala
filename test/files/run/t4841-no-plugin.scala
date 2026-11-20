
import tools.partest.DirectTest

import java.io.File

// warn if no plugin on Xplugin path, whether or not it exists; -Xpluginsdir is not required to have plugins
object Test extends DirectTest {
  override def code = "class Code"

  override def show() = {
    val tmp = new File(testOutput.jfile, "plugins.partest").getAbsolutePath
    val empty = new File(testOutput.jfile, "empty.partest")
    assert(empty.mkdir())
    compile(s"-Xplugin:$tmp", s"-Xplugin:${empty.getAbsolutePath}", "-Xpluginsdir", empty.getAbsolutePath)
  }
}

