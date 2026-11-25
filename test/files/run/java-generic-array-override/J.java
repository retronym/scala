public class J<T> { public String g(T[] xs) { return "J.g"; }
  public static <T> String call(J<T> j, T[] xs) { return j.g(xs); } }
