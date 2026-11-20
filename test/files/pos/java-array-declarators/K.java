import java.util.HashMap;
import java.util.Map;

public class K {
  static int f(int x, int y) { return x + y; }
  static int[] y = {1};
  static <A, B, C> Object g() { return null; }

  public int a1, b1[], c1;
  public int[] arr = {1}, arr2[] = {{1}}, arr3;
  public int d1, e1[][], g1[];
  public long h1[] = {1L, 2L}, i1[][] = {{1L}, {2L}}, j1 = 1L;

  // commas in an initializer that are followed by `x[`
  public int k1 = f(1, y[0]), l1;
  public int[] m1 = {y[0], y[0]}, n1[];
  public Map<String, int[]> o1 = new HashMap<String, int[]>(), p1[];
  public Map<String[], String[]> q1 = new HashMap<String[], String[]>(), r1;
  public Object s1 = K.<String[], String[], String>g(), t1;
}
