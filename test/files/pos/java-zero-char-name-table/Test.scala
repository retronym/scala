//> using options -Yname-table:string
// The Java scanner gives '\0' an empty name; reading its first char used to throw with a String-backed name.
object Test {
  val zero: Char = Zero.Zero
}
