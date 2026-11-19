public class V {
  @SafeVarargs public static <B extends CharSequence> int f(B... bs) { return bs.length; }
  @SafeVarargs public static <B extends Number & Comparable<B>> int g(String s, B... bs) { return bs.length; }
  @SafeVarargs public static <B> int h(B... bs) { return bs.length; }
  public static int p(int... xs) { return xs.length; }

  public static int callScala() { return S.s() + S.s("a"); }
}
