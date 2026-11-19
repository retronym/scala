public class J {
  @Deprecated(forRemoval = true, since = "2.5") public static void removal() {}
  @Deprecated(since = "3.0", forRemoval = false) public static void since() {}
  @Deprecated(forRemoval = true) public static void removalOnly() {}
  @Deprecated public static void plain() {}

  @Deprecated(forRemoval = true, since = "1.1") public static class C {}
  @Deprecated(forRemoval = true, since = "1.2") public static int field = 0;
}
