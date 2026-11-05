package pneg;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@interface Marker { Class<?> value() default Object.class; int n() default 1; }

@Marker(value = String.class, n = 2)
public class A07Ok {
  public enum Color { RED, GREEN, BLUE }
  public static final int K = 1 + 2;
  public static final String S = "s";
  transient int t;
  volatile long v;
  native void nat();
  strictfp double d(double x) { return x; }
  <T extends Comparable<T>> T max(T a, T b) { return a; }
  int[] arr(boolean b, byte y, short s, char c, long l, float f, double d) { return null; }
}
