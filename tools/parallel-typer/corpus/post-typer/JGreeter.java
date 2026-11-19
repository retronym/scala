package ptj;

public interface JGreeter {
  default String greet(String who) { return "hello " + who; }
  String name();
}
