package lp;

public interface JIface<T> {
  T get();
  default String describe(T t) { return "J:" + t; }
  static int twice(int x) { return x * 2; }
}
