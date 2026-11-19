// A `return` nested anywhere inside the initializer of a local lazy val returns
// from the enclosing method, not from the `x$lzycompute` method that the
// initializer is moved into.
object Test {
  def direct: Int = { lazy val x: Int = { return 1 }; x + 100 }

  def inVal: Int = { lazy val x: Int = { val y: Int = { return 2 }; y + 1 }; x + 100 }

  def inValString: String = { lazy val x: Int = { val y: Int = { return "three" }; y + 1 }; "v" + x }

  def inVar: Int = { lazy val x: Int = { var y: Int = { return 4 }; y += 1; y }; x + 100 }

  def inNestedBlocks: Int = {
    lazy val x: Int = {
      val y: Int = {
        val z = { { if (inVal > 0) return 5 }; 1 }
        z
      }
      y + 1
    }
    x + 100
  }

  def inPatternVal: Int = {
    lazy val x: Int = {
      val (a, b) = (1, { if (inVal > 0) return 6; 2 })
      a + b
    }
    x + 100
  }

  def inNestedLazyVal: Int = {
    lazy val x: Int = {
      lazy val y: Int = { val z: Int = { return 7 }; z }
      y + 1
    }
    x + 100
  }

  def inLocalDef: Int = {
    lazy val x: Int = {
      // returns from `local`, not from `inLocalDef`
      def local: Int = { val y: Int = { return 8 }; y + 1 }
      local
    }
    x + 100
  }

  def inLazyValOfLocalDef: Int = {
    def local: Int = { lazy val x: Int = { val y: Int = { return 9 }; y + 1 }; x + 100 }
    local + 1000
  }

  def inTry: Int = {
    lazy val x: Int = {
      val y: Int = try { return 10 } finally ()
      y + 1
    }
    x + 100
  }

  def unit(sb: StringBuilder): Unit = {
    lazy val x: Int = { val y: Int = { sb.append("returning"); return }; y + 1 }
    sb.append(x)
  }

  def main(args: Array[String]): Unit = {
    println(direct)
    println(inVal)
    println(inValString)
    println(inVar)
    println(inNestedBlocks)
    println(inPatternVal)
    println(inNestedLazyVal)
    println(inLocalDef)
    println(inLazyValOfLocalDef)
    println(inTry)
    val sb = new StringBuilder; unit(sb); println(sb)
  }
}
