import java.util.function.Supplier;

// a bound that is a generic class: the erased types are the raw `Supplier[]` and `Supplier`
@SuppressWarnings("unchecked")
public class JC<T extends Supplier<String>> {
  public String g(T... xs) { return "JC.g" + xs.length; }
  public T r(T... xs) { return null; }
  public String h(T a, T... xs) { return "JC.h" + xs.length; }
  public String callG(T a) { return g(a, a); }
  public Object callR(T a) { return r(a, a); }
  public String callH(T a) { return h(a, a); }
}
