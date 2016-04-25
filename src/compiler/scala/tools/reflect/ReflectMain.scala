package scala.tools
package reflect

import scala.reflect.internal.util.ScalaClassLoader
import scala.tools.nsc.Driver
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import scala.tools.util.PathResolverBase

object ReflectMain extends Driver {

  private def classloaderFromSettings(settings: Settings) = {
    val classPathURLs = new PathResolverBase(settings).resultAsURLs
    ScalaClassLoader.fromURLs(classPathURLs, getClass.getClassLoader)
  }

  override def newCompiler(): Global = new ReflectGlobal(settings, reporter, classloaderFromSettings(settings))
}
