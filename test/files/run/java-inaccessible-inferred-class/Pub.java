package p;

// Hid is package-private; public methods of Pub expose it.
class Hid {
  public Hid self() { return this; }
  public java.util.List<Hid> list() { return java.util.Collections.singletonList(this); }
}

public class Pub extends Hid { }
