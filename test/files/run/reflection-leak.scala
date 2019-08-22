import java.util.concurrent.{Callable, ExecutorService, Executors}

import scala.tools

// this test checks that under heavily multithreaded conditions:
// 1) scala.reflect.runtime.universe, its rootMirror and definitions are initialized correctly
// 2) symbols are correctly materialized into PackageScopes (no dupes)
// 3) unpickling works okay even we unpickle the same symbol a lot of times

object Test extends App {

  val executors = Executors.newFixedThreadPool(16)
  try {
    for (i <- 1 to 1024) {
      print(".")
      val g = new scala.reflect.runtime.JavaUniverse {
      }

      try {
        import g.rootMirror.typeOf
        import g._

        def foo[T: g.TypeTag](x: T) = typeOf[T].toString

        val n = 16
        val rng = new scala.util.Random()
        val types = List(
          () => typeOf[java.lang.reflect.Method],
          () => typeOf[java.lang.annotation.Annotation],
          () => typeOf[scala.io.BufferedSource],
          () => typeOf[scala.io.Codec])
        val perms = types.permutations.toList

        def force(lazytpe: () => Type): String = {
          lazytpe().typeSymbol.info.members.foreach(_.info)
          lazytpe().toString
        }

        val diceRolls = List.fill(n)(rng.nextInt(perms.length))
        val futures = (1 to n).map { (i: Int) =>
          executors.submit(new Callable[Object]() {
            override def call(): Object = {
              val s1 = foo("42")
              val s2 = perms(diceRolls(i - 1)).map(x => force(x)).sorted.mkString(", ")
              assert(s1 == "String" || s1 == "java.lang.String")
              assert(s2 == "java.lang.annotation.Annotation, java.lang.reflect.Method, scala.io.BufferedSource, scala.io.Codec")
              null
            }
          })
        }
        futures.foreach(_.get())
      } finally {
        g.close()
        System.gc()
        System.gc()
        System.gc()
      }
    }
  } finally {
    executors.shutdownNow()
  }
  println()
  println("sleeping")
  // Memory is not released here.
  Thread.sleep(100000)
}
