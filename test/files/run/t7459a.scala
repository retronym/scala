class LM {
  class Node

  // crash
  (this: LM) match {
    case tttt =>
      new tttt.Node()
      tttt
  }
}

object Test extends App {
  new LM()
}