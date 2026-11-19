package stress

import java.{util => ju}

object Use29 {
  // raw Java types and members from the classpath, cooked on first use (TRIEDCOOKING)
  def raw(): Int = {
    val l = new ju.ArrayList[AnyRef]()
    l.add("x29"); ju.Collections.reverse(l)
    val m = new ju.HashMap[String, Integer](); m.put("k", 29)
    val c: ju.Comparator[String] = ju.Comparator.naturalOrder[String]()
    val s = new ju.TreeSet[String](c); s.add("a")
    val it = l.iterator(); var n = 0; while (it.hasNext) { it.next(); n += 1 }
    n + m.size + s.size + ju.Objects.hash("a", "b") + new java.lang.StringBuilder("q").length
  }
  // the sealed hierarchies of another unit, matched and copied here
  def area(s: Shape): Double = s match {
    case Circle(r)  => r * r * 3
    case Rect(w, h) => w * h
    case Empty      => 0
  }
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)    => 1
    case Node(l, r) => size(l) + size(r)
  }
  def more(c: Circle) = c.copy(r = c.r * 29) == Circle(1) || c.productIterator.nonEmpty || Rect.unapply(Rect(1, 2)).isDefined
  def all = List(Circle(1), Rect(1, 2), Empty).map(area).sum + size(Node(Leaf(1), Leaf(29)))
}
