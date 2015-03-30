public class A_1 {}

interface Component {
	public ListModel getModel();
}
interface ListModel {}

interface Base<T> {
  Component peer();
}

interface B<T> extends Base<T> {
  Component peer();
}
