package a;

// redeclares `J.foo` with package access: the redeclaration is not overridden from another package either
public abstract class K3 extends J {
  abstract void foo();
}
