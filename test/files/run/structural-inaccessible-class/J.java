public class J {
  private static class Impl implements Runnable {
    public void run() { System.out.println("Impl.run"); }
  }
  static class Hidden {
    public int answer() { return 42; }
  }
  public static Runnable impl() { return new Impl(); }
  public static Object hidden() { return new Hidden(); }
}
