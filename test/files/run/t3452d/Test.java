import scala.collection.immutable.Nil;
import scala.collection.immutable.List;
import scala.collection.Traversable;

public class Test {
	public static void main(String[] args) {
		C<String> c = new C<String>();
		Object ls = c.tail();
        // TODO should mixin give this a more refined signature?
	}
}
