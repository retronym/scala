public interface F3 {
  String join(String sep, Object... xs);

  static String call(F3 f) { return f.join("-", "a", "b"); }
}
