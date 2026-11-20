public interface VS<T> {
  String m(T... ts);

  static String callStr(VS<String> v) { return v.m("a", "b"); }
  static String callArr(VS<String> v) { return v.m(new String[] { "x" }); }
  static String callInt(VS<Integer> v) { return v.m(1, 2); }
}
