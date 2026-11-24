public class V {
  @SafeVarargs public static <B extends CharSequence> int f(B... bs) { return bs.length; }
  @SafeVarargs public static <B extends Number & Comparable<B>> int g(String s, B... bs) { return bs.length; }
  @SafeVarargs public static <B> int h(B... bs) { return bs.length; }
  public static int p(int... xs) { return xs.length; }
  // the element type erases to a generic class
  @SafeVarargs public static <K extends Comparable<K>, B extends java.util.List<K>> int l(B... bs) { return bs.length; }
  @SafeVarargs public static <B extends Enum<B>> int e(B... bs) { return bs.length; }

  public static int callScala() { return S.s() + S.s("a"); }
}
