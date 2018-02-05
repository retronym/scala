/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import io.VirtualDirectory
import settings.MutableSettings
import scala.reflect.io._
import scala.collection.generic.Clearable

/** Directory to save .class files to. */
trait ReplDir extends AbstractFile with Clearable { }

private class ReplRealDir(dir: Directory) extends PlainDirectory(dir) with ReplDir {
  def clear() = {
    dir.deleteRecursively()
    dir.createDirectory()
  }
}

class ReplOutput(val dirSetting: MutableSettings#StringSetting) {
  // outdir for generated classfiles - may be in-memory (the default),
  // a generated temporary directory, or a specified outdir.
  val dir: AbstractFile = (
    if (dirSetting.isDefault) {
      new PlainNioFile(VirtualDirectory.newAnonymousNioVirtualDirectory().getPath("/"))
    } else if (dirSetting.value == "")
      new ReplRealDir(Directory.makeTemp("repl"))
    else
      new ReplRealDir(Directory(dirSetting.value))
  )

  // print the contents hierarchically
  def show(out: JPrintWriter) = {
    def pp(root: AbstractFile, indentLevel: Int) {
      val label = root.name
      val spaces = "    " * indentLevel
      out.println(spaces + label)
      if (root.isDirectory)
        root.toList sortBy (_.name) foreach (x => pp(x, indentLevel + 1))
    }
    pp(dir, 0)
  }
}
