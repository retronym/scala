@SuppressWarnings("unchecked")
public class JW<T> {
  public <U> String k(T t, U... us) { return "JW.k"; }
  public String callK(T t) { return k(t, 1, 2); }
}
