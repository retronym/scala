// A try/catch as the receiver of an operation for which erasure evaluates something else before the receiver:
// ScalaRunTime.op(receiver, ...), or `path eq receiver` for a test against a singleton type.
object Test {
  def genericApply[T](a: Int, x: Array[T], y: Array[T]): T =
    (try { if (a > 0) throw new Exception; x } catch { case _: Exception => y })(0)
  def genericLength[T](a: Int, x: Array[T], y: Array[T]): Int =
    (try { if (a > 0) throw new Exception; x } catch { case _: Exception => y }).length
  def genericUpdate[T](a: Int, x: Array[T], y: Array[T], t: T): Unit =
    (try { if (a > 0) throw new Exception; x } catch { case _: Exception => y })(0) = t
  def genericClone[T](a: Int, x: Array[T], y: Array[T]): Array[T] =
    (try { if (a > 0) throw new Exception; x } catch { case _: Exception => y }).clone
  def abstractLength[A <: Array[Int]](a: Int, x: A, y: A): Int =
    (try { if (a > 0) throw new Exception; x } catch { case _: Exception => y }).length
  def primitiveGetClass(a: Int): Class[Int] =
    (try { if (a > 0) throw new Exception; 1 } catch { case _: Exception => 2 }).getClass
  def isArray(a: Int, x: Any): Boolean =
    (try { if (a > 0) throw new Exception; x } catch { case _: Exception => "s" }).isInstanceOf[Array[_]]
  def isArrayOfArray(a: Int, x: Any): Boolean =
    (try { if (a > 0) throw new Exception; x } catch { case _: Exception => "s" }).isInstanceOf[Array[Array[_]]]
  def isSingleton(a: Int, x: Any, p: AnyRef): Boolean =
    (try { if (a > 0) throw new Exception; x } catch { case _: Exception => "s" }).isInstanceOf[p.type]
  def isSingletonAlias(a: Int, x: Any, p: AnyRef): Boolean = {
    type P = p.type
    (try { if (a > 0) throw new Exception; x } catch { case _: Exception => "s" }).isInstanceOf[P]
  }
  def isThis(a: Int, x: Any): Boolean =
    (try { if (a > 0) throw new Exception; x } catch { case _: Exception => "s" }).isInstanceOf[this.type]
  def isLiteral(a: Int, x: Any): Boolean =
    (try { if (a > 0) throw new Exception; x } catch { case _: Exception => "s" }).isInstanceOf["s"]
  def isCompound(a: Int, x: Any, p: AnyRef): Boolean =
    (try { if (a > 0) throw new Exception; x } catch { case _: Exception => "s" }).isInstanceOf[p.type with Serializable]
  // erasure leaves these as an instance test of the receiver
  def isList(a: Int, x: Any): Boolean =
    (try { if (a > 0) throw new Exception; x } catch { case _: Exception => Nil }).isInstanceOf[List[_]]

  def main(args: Array[String]): Unit =
    for (a <- 0 to 1) {
      println(s"genericApply($a) = ${genericApply(a, Array("x"), Array("y"))}")
      println(s"genericLength($a) = ${genericLength(a, Array("x"), Array("y", "z"))}")
      val (x, y) = (Array("x"), Array("y"))
      genericUpdate(a, x, y, "updated")
      println(s"genericUpdate($a) = ${x(0)} ${y(0)}")
      println(s"genericClone($a) = ${genericClone(a, Array("x"), Array("y")).mkString}")
      println(s"abstractLength($a) = ${abstractLength(a, Array(1), Array(1, 2))}")
      println(s"primitiveGetClass($a) = ${primitiveGetClass(a)}")
      println(s"isArray($a) = ${isArray(a, Array(1))}")
      println(s"isArrayOfArray($a) = ${isArrayOfArray(a, Array(Array(1)))}")
      println(s"isSingleton($a) = ${isSingleton(a, "s", "s")}")
      println(s"isSingletonAlias($a) = ${isSingletonAlias(a, "x", "s")}")
      println(s"isThis($a) = ${isThis(a, this)}")
      println(s"isLiteral($a) = ${isLiteral(a, "x")}")
      println(s"isCompound($a) = ${isCompound(a, "x", "s")}")
      println(s"isList($a) = ${isList(a, "x")}")
    }
}
