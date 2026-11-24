// A Java `T[]` is a reference array even when Scala instantiates `T` to a primitive value class.
public class Y {
  @SafeVarargs public static <T> T[] arr(T... ts) { return ts; }
  public static <T> T[][] nested(T t) { @SuppressWarnings("unchecked") T[][] a = (T[][]) new Object[][] { { t } }; return a; }
  public static class Box<T> {
    private final T t;
    public Box(T t) { this.t = t; }
    @SuppressWarnings("unchecked") public T[] items() { return (T[]) new Object[] { t, t }; }
  }
}
