class C[@specialized(Int) T](val t: T) {
  // a method of a specialized class, duplicated into C$mcI$sp
  def inClass(x: T): String = {
    trait TT { println("init"); val v: T = x; private val w: String = "w"; def get = s"$v $w" }
    (new TT {}).get
  }
}

object Test {
  def run[@specialized(Int) T](t: T): Int = {
    trait TT { val v: Int = 8 }
    (new TT {}).v
  }
  def main(args: Array[String]): Unit = {
    println(run(0))   // specialized copy run$mIc$sp
    println(run(""))  // generic copy
    println(new C(1).inClass(2))
    println(new C("a").inClass("b"))
  }
}
