trait U {
  val global: SymTab
  object o { def apply(a: Any) = ()}
  object p
}

trait SymTab

trait T { self: SymTab =>
  def e: U { val global: T.this.type} = ???
  e.o(null) // error: Eras{val global: <refinement>.type}#<none> does not take parameters
}
