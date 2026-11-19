package t11109;

import java.io.*;

// Java erases `T...` to an array of the erasure of `T`'s first bound: `Serializable[]`, `Comparable[]`, etc.
// Scala must call the method with that descriptor, although `Serializable`, `Comparable` and `Cloneable`
// are universal traits in Scala (`Comparable[U] <:< AnyRef` is false).
public class JaVarArgs {
  @SafeVarargs public final <T extends Serializable> int serialize(T... ts) { return ts.length; }
  @SafeVarargs public final <T extends Universal> int universalize(T... ts) { return ts.length; }
  @SafeVarargs public static <U extends Comparable<U>> int cmp(U... us) { return us.length; }
  @SafeVarargs public static <U extends Comparable<? super U>> int cmpSuper(U... us) { return us.length; }
  @SafeVarargs public static <U extends Comparable<?>> int cmpWild(U... us) { return us.length; }
  @SafeVarargs public static <U extends Cloneable> int cln(U... us) { return us.length; }
  @SafeVarargs public static <U extends V, V extends Comparable<V>> int twoLevel(U... us) { return us.length; }
  @SafeVarargs public static <U extends V, V> int twoLevelUnbounded(U... us) { return us.length; }
  @SafeVarargs public static <U> int unbounded(U... us) { return us.length; }

  public static class Ctor<T extends Comparable<T>> {
    public final int n;
    @SafeVarargs public Ctor(T... ts) { n = ts.length; }
  }

  // Implemented in Scala
  public interface I {
    <U extends Comparable<U>> int m(U... us);
  }
  public static abstract class AC<T extends Comparable<T>> {
    public abstract int n(T... ts);
  }
  public static int callI(I i) { return i.m("a", "b"); }
  public static int callAC(AC<String> ac) { return ac.n("a", "b"); }
}
