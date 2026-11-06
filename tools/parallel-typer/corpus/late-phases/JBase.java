package lp;

import java.util.List;

public abstract class JBase<A extends Comparable<A>> implements JIface<A> {
  protected int count;
  public static final int LIMIT = 16;
  public abstract List<A> items();
  protected String prot(String s, int... xs) { return s + xs.length; }
  public static class Nested { public int n = 1; public class Deep { public int m = n + 1; } }
  public enum Colour { RED, GREEN, BLUE }
  public List raw() { return items(); }
}
