import java.nio.file.*;
import java.util.*;

/** Writes srcs/<shape><N>.scala: the same source for Scala 2 and Scala 3. */
public class Gen {
  // `R` shapes: the second field has the sealed type itself, as in retronym/scala3#23's PatmatBenchmark
  static String sealedDefs(int n, boolean rec) {
    StringBuilder sb = new StringBuilder("sealed trait T\n");
    for (int i = 0; i < n; i++) sb.append("final case class C").append(i).append(rec ? "(a: Int, b: T) extends T\n" : "(a: Int, b: Int) extends T\n");
    return sb.toString();
  }
  static String src(String shape0, int n) {
    boolean rec = shape0.endsWith("R");
    String shape = rec ? shape0.substring(0, shape0.length() - 1) : shape0;
    StringBuilder b = new StringBuilder();
    boolean sealed = !shape.equals("ints");
    switch (shape) {
      case "ctor":  b.append("def f(t: T): Int = t match {\n"); for (int i = 0; i < n; i++) b.append("  case C" + i + "(_, _) => " + i + "\n"); break;
      case "typ":   b.append("def f(t: T): Int = t match {\n"); for (int i = 0; i < n; i++) b.append("  case _: C" + i + " => " + i + "\n"); break;
      case "tup":   b.append("def f(a: T, b: T): Int = (a, b) match {\n"); for (int i = 0; i < n; i++) b.append("  case (C" + i + "(_, _), C" + i + "(_, _)) => " + i + "\n"); b.append("  case _ => -1\n"); break;
      case "opt":   b.append("def f(o: Option[T]): Int = o match {\n"); for (int i = 0; i < n; i++) b.append("  case Some(C" + i + "(_, _)) => " + i + "\n"); b.append("  case None => -1\n"); break;
      case "guard": b.append("def f(t: T, g: Boolean): Int = t match {\n"); for (int i = 0; i < n; i++) b.append("  case C" + i + "(_, _) if g => " + i + "\n"); b.append("  case _ => -1\n"); break;
      case "seq":   b.append("def f(s: Vector[T]): Int = s match {\n  case Vector() => -1\n"); for (int i = 0; i < n; i++) b.append("  case C" + i + "(_, _) +: _ => " + i + "\n"); b.append("  case _ => -2\n"); break;
      case "ints":  b.append("def f(x: Int): Int = x match {\n"); for (int i = 0; i < 2 * n; i++) b.append("  case " + i + " => " + i + "\n"); b.append("  case _ => -1\n"); break;
      default: throw new IllegalArgumentException(shape);
    }
    b.append("}\n");
    return (sealed ? sealedDefs(n, rec) : "") + "object O {\n" + b + "}\n";
  }
  public static void main(String[] args) throws Exception {
    for (String shape : args[0].split(",")) for (String n : args[1].split(","))
      Files.writeString(Path.of(args[2], shape + n + ".scala"), src(shape, Integer.parseInt(n)));
  }
}
