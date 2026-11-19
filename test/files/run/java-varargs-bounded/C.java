public class C<B extends CharSequence> {
  @SafeVarargs public final int f(B... bs) { return bs.length; }
}
