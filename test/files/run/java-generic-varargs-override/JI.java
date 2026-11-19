@SuppressWarnings("unchecked")
public interface JI<T> {
  String g(T... xs);
  static <T> String call(JI<T> j, T a) { return j.g(a, a); }
}
