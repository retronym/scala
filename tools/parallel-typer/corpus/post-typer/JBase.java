package ptj;

// protected members reached from Scala code in another package: closures and inner classes there need accessors
public class JBase {
  protected int counter = 0;
  protected static int shared = 0;
  protected String describe(String prefix) { return prefix + counter; }
  protected <T> T pick(T a, T b) { return counter % 2 == 0 ? a : b; }
  protected static String label() { return "JBase"; }
  public int bump() { counter += 1; return counter; }
}
