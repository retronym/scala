// Mixing a Java interface into a Scala class is the Scala class's doing, so the usual rules apply.
class S { var x: Int = 1 }
class C extends S with JI
