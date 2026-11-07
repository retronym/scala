package ptj;

// a Java source that refers to the Scala classes of this run
public class JUsesScala extends pt.Base {
  public pt.Meters twice(pt.Named n) { return null; }
  public static int answer() { return new pt.Base().m(41) + 1; }
  @Override public int m(int x) { return super.m(x) * 2; }
}
