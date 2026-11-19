package j;

public class JP_1<T> {
  protected static int sf = 10;
  protected static String sm(int x) { return "sm" + x; }
  protected static String sz() { return "sz"; }
  protected static <U> U sg(U u) { return u; }
  protected static String sv(String... xs) { return "sv" + xs.length; }
}
