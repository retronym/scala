object Test {
  (null: Any) match {
    case Extractor(otherExRep) => 
      println(otherExRep)
  }

  trait T {
    type U 
  }

  object Extractor {
    def unapply(t: T): Option[t.U] = ???
  }
}

