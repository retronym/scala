public class B_2 {
  public static void run() {
    A a = new A();
    try { System.out.println(a.poly1(1)); } catch (NoSuchMethodError e) { System.out.println("NoSuchMethodError " + e.getMessage()); }
    try { System.out.println(((int[]) a.arr1(new int[] {1})).length); } catch (NoSuchMethodError e) { System.out.println("NoSuchMethodError " + e.getMessage()); }
    try { System.out.println(a.polyMono(new Mono(1)).value()); } catch (NoSuchMethodError e) { System.out.println("NoSuchMethodError " + e.getMessage()); }
    try { System.out.println(a.polyPoly(new Poly<Object>(1)).value()); } catch (NoSuchMethodError e) { System.out.println("NoSuchMethodError " + e.getMessage()); }
    scala.collection.immutable.List<String> xs = a.polyList(null);
    System.out.println(xs);
  }
}
