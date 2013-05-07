object Test {
  trait Universe {
    type Type
    type TypeTag[A] >: Null <: TypeTagApi[A]
    trait TypeTagApi[A] { def tpe: Type }
  }
  trait JavaUniverse extends Universe

  trait Mirror[U <: Universe] {
    def universe: U
  }
  (null: Mirror[_]).universe match {
    case ju: JavaUniverse => 
      val ju1 = ju
      val f = {() => (null: ju.TypeTag[Nothing]).tpe }
  }
  trait M[A]
}

/*
  PENDING:
  error: Inferred type x2.Type contains type selection from volatile type Test.<refinement>.type
      val f = {() => (null: ju.TypeTag[Nothing]).tpe }
                                                 ^
/*