package p;

// Hid is package-private; Pub.self() is public and returns it.
class Hid extends Base {
  public Hid self() { return this; }
}

public class Pub extends Hid { }
