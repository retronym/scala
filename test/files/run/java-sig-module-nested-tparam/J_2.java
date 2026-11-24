public class J_2 {
  public static String test() {
    // the type variable of the outer class is visible in a member of the module class
    String s1 = new O1<String>("1").ob().t();
    // but not in classes nested in the module class, where it is erased
    Object s3 = new O3<String>("3").e().t();
    scala.collection.immutable.List<?> l3 = new O3<String>("3").e().l();
    String s4 = new O4<String>("4").ob().t();
    Object s5 = new O5<String>("5").oc().t();
    Object s6 = new O6<String>("6").f().t();
    String u6 = new O6<String>("6").f().u();
    java.util.List<?> asList = new O6<String>("6").f();
    return s1 + s3 + l3.head() + s4 + s5 + s6 + u6 + asList.size();
  }
}
