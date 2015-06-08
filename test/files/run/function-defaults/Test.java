public class Test {
	static void assertTrue(boolean b) {
		if (!b) throw new AssertionError();
	}
	@SuppressWarnings("unchecked")
    public static void main(String[] args) {
        scala.Function0<String> f0_s = (()-> "s");
        assertTrue(f0_s.apply().equals("s"));

        scala.Function1<String, String> f1_ss = (x -> x);
        assertTrue(f1_ss.apply("").equals(""));

        scala.Function0$mcI$sp f0_i = () -> 42;
        assertTrue(f0_i.apply().equals(42));
        assertTrue(f0_i.apply$mcI$sp() == 42);
        scala.Function0 f0_raw = f0_i;
        assertTrue(f0_raw.apply().equals(Integer.valueOf(42)));

        scala.Function1$mcII$sp f1_ii = (x -> -x);
        scala.Function1 f1_raw = f1_ii;
        assertTrue(f1_raw.apply(1).equals(Integer.valueOf(-1)));
    }
}
