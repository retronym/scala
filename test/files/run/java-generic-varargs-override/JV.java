@SuppressWarnings("unchecked")
public class JV<T> {
  public String g(T... xs) { return "JV.g" + xs.length; }
  public <U> String m(U... xs) { return "JV.m" + xs.length; }
  public T r(T... xs) { return null; }
  public String h(T a, T... xs) { return "JV.h" + xs.length; }
  @SafeVarargs public final String callG(T... xs) { return g(xs); }
  public String callG1(T a) { return callG(a); }
  public String callM(Object a) { return m(a, a); }
  public String callH(T a) { return h(a, a); }
  public Object callR(T a) { return r(a, a); }
}
