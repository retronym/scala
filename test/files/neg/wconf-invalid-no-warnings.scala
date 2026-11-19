//> using options -Wconf:cat=bogus:e -quickfix:cat=nope

// The filters are invalid, and nothing here warns.
class C {
  def f = 1
}
