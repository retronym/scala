public class B_2 {
  static class OneTwo implements One, Two {}
  static class KOne extends K implements Two {}
  static class KKT extends K implements KT {}
  static class LOne extends L implements One {}

  public static void run() {
    A a = new A();
    System.out.println(a.foo1(new One() {}));
    System.out.println(a.foo2(new OneTwo()));
    System.out.println(a.foo3(new Sub() {}));
    System.out.println(a.foo4(new KKT()));
    System.out.println(a.foo5(new KOne()));
    System.out.println(a.foo6(new LOne()));
    System.out.println(a.foo7("x"));
    System.out.println(a.foo8(new One() {}));
  }
}
