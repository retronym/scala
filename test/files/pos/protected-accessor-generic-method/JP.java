package j;

public class JP<T> {
  protected <U extends T> U g(U u, T t) { return u; }
}
