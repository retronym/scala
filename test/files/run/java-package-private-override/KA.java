package a;

// redeclares `JA.foo` with wider access: a subclass in another package overrides `JA.foo` through it
public abstract class KA extends JA {
  public abstract String foo();
}
