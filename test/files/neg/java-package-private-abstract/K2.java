package a;

// redeclares `J.foo` with wider access, abstract: overrides `J.foo`, and is overridden from any package
public abstract class K2 extends J {
  public abstract void foo();
}
