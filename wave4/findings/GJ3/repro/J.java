public class J {
  @Deprecated(since = "1" + "2", forRemoval = false) public static int a() { return 1; }
  @Deprecated(since = "12") public static int b() { return 2; }
  public @Deprecated static int c() { return 3; }
  @Deprecated(forRemoval = false) public static int d() { return 4; }
}
