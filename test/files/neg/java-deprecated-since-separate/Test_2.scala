//> using options -deprecation -Werror
//> using jvm 9+

// `since` is not the first element of `@Deprecated(forRemoval = true, since = "2.5")`
class Test {
  J_1.removal()
  J_1.since()
  J_1.removalOnly()
  J_1.plain()
  new J_1.C
  J_1.field
}
