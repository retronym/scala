import p.*;

public class J_2 {
  @SuppressWarnings("unchecked")
  public static void run() {
    VG<String>[] vgs = (VG<String>[]) new VG[] { new VG<String>("x") };
    System.out.println(Sigs.arrVG(vgs)[0].t());
    Object[] nulls = Sigs.arrNothing(new Object[0], "t");
    System.out.println(nulls.length);
  }
}
