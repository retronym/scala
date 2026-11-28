@SuppressWarnings("unchecked")
public class JB<T extends CharSequence> {
  public String g(T... xs) { return "JB.g" + xs.length; }
  @SafeVarargs public final String callG(T... xs) { return g(xs); }
  public String callG1(T a) { return callG(a); }
  public String callGS(T a) { return g(a); }
}
