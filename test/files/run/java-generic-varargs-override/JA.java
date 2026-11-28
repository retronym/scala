@SuppressWarnings("unchecked")
public abstract class JA<T> {
  public abstract String a(T... xs);
  public String callA(T x) { return a(x, x); }
}
