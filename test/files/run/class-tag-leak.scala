import java.net

import scala.reflect.internal.util.ScalaClassLoader.{URLClassLoader, setContext}
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.runtime.ScalaRunTime

object Repro {
  val big = Array.ofDim[Byte](32 * 1024 * 1024)
  val cls = getClass
  val tag = scala.reflect.ClassTag.apply(cls)
  System.out.println(s"${System.identityHashCode(cls)} ${System.identityHashCode(tag)} ${System.identityHashCode(tag.getClass)}")
}

object Test {

  def main(args: Array[String]): Unit = {
    val urls = getClass.getClassLoader.asInstanceOf[net.URLClassLoader].getURLs
    for (i <- 1 to 1) {
      val cl = new java.net.URLClassLoader(urls, null) {
        override def loadClass(name: String, resolve: Boolean): Class[_] = {
          if (name.startsWith("scala")) {
            // don't parent delegate, load a fresh Class[_] instance
            val c = findClass(name)
            if (resolve) {
              resolveClass(c)
            }
            c
          } else super.loadClass(name, resolve)
        }
      }
      try {
        for (j <- 1 to 1) {
          val reproModuleClassName = "Repro$"
          val cl1 = new java.net.URLClassLoader(urls, cl) {
            override def loadClass(name: String, resolve: Boolean): Class[_] = {
              if (name == reproModuleClassName) {
                // don't parent delegate, load a fresh Class[_] instance
                val c = findClass(name)
                if (resolve) {
                  resolveClass(c)
                }
                c
              } else super.loadClass(name, resolve)
            }
          }
          try {
            val cls = cl1.loadClass(reproModuleClassName)
            cls.getDeclaredField("MODULE$").get(null)
          } finally {
            cl1.close()
          }
        }
      } finally {
        cl.close()
      }
    }
  }
}
