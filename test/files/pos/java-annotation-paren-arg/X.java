public class X {
  @interface A { int value(); }
  @interface B { int[] value(); }
  @interface C { A a(); int n() default 0; }

  @A((int) 2L) public void m() { }
  @A(value = (1)) public int f;
  @A((1 + 2) * (3)) public int g;
  @A(1 + (2)) public int h;
  @A(-(1)) public int i;
  @B({(1), (2)}) public int j;
  @B({(int) 1L, 2}) @A(1) public int k;
  @B(value = {1, (2)}) public int l;
  @C(a = @A((1)), n = (2)) public int n;
  @C(n = (2), a = @A((int) 1L)) public int o;
  public void p(@A((int) 2L) int x, @A((1)) int y) { }
  @A((1)) public static class Q { }
}
