object Test
  // Worked in 2.11.0-M5
  {
    val a: Option[Int] = None

    (a: a.type) match {
       case s: Some[Int] => println(s)
       case None    =>
    }
  }

  // Worked in 2.11.0-M5
  {
    val a: Option[Int] = None

    (a: a.type) match {
       case Some(x) => println(x)
       case None    =>
    }
  }

  // Has never worked, but seems desirable given the recent changes to
  // pattern type inference.
  {
   val a = ""
   object Id {
      def unapply(xxxx: Any): Some[a.type] = Some[a.type](a)
    }
    val b: a.type = (a: a.type) match {
      case Id(x) => x
    }
  }
}
