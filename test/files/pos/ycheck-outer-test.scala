//> using options -Ycheck:patmat,uncurry,fields

// The outer test of the type pattern in `equals` selects a synthetic `<outer>` method until explicitouter.
class Outer { case class CC(x: Int) }
