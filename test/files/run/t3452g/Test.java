
public class Test {
	public static void main(String[] args) {
		// Requires addition of bridge during mixin so we can expose
		// a generic return type of Traversable<A>, because the erasure
		// of this (Traversable) differs from the erasure of the mixed
		// method (erasure(Repr) = Object)

		// TODO why doesn't Java substitute A=String here?
		//      Do we need another bridge in object O to provide
		//      a sharper generic signature.
		AbstractTrav<Object> lsSharp = O.tail();

		AbstractTrav<String> lsSharp2 = new C<String>().tail();
	}
}
