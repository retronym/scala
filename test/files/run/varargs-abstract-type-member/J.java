public class J {
  public static void run() {
    Strings s = new Strings();
    System.out.println(s.unbounded("a", "b"));
    System.out.println(s.refBounded("a", "b", "c"));
    System.out.println(s.valBounded(1));
    System.out.println(s.lowerBounded());
    System.out.println(s.last("a", "b", "c"));

    A a = new Ints();
    System.out.println(a.unbounded(1, 2, 3));
    System.out.println(a.last(1, 2, 3));

    System.out.println(new Outer<String>().alias("a", "b"));
  }
}
