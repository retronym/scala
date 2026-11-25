import java.lang.reflect.Modifier

class Box[@specialized(Int, Long, Double) T](val t: T)

// The instance fields of a specialized instance, including those it inherits. An instance needs one field for
// each value it holds.
object Test {
  def instanceFields(o: AnyRef): List[String] = {
    def loop(c: Class[_]): List[String] =
      if (c == null) Nil
      else loop(c.getSuperclass) ::: c.getDeclaredFields.toList.filterNot(f => Modifier.isStatic(f.getModifiers)).sortBy(_.getName).map(f => s"${f.getName}: ${f.getType.getSimpleName}")
    loop(o.getClass)
  }

  def show(o: AnyRef): Unit = println(s"${o.getClass.getName}: ${instanceFields(o).mkString(", ")}")

  def main(args: Array[String]): Unit = {
    show(("a", "b"))
    show((1, 2))
    show((1.0, 2.0))
    show((1L, 2))
    show(Tuple1(1))
    show(new Box("a"))
    show(new Box(1))
    show(new Box(1L))
  }
}
