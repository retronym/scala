class Outer {
  def apply( position : Inner ) {}
  class Inner

  this (new Inner) // error,
}


class Outer1 {

  self =>

  def apply( position : Inner ) : Boolean = true

  class Inner( ) {

    def testMe = {
      self.apply( this ) // a) this works
      self( this ) // b) this does not work!
    }
  }
}