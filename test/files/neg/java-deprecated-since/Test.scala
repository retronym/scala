//> using options -deprecation -Werror
//> using jvm 9+

// `since` is not the first element of `@Deprecated(forRemoval = true, since = "2.5")`
class Test {
  J.removal()
  J.since()
  J.removalOnly()
  J.plain()
  new J.C
  J.field
}
