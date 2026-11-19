//> using options -Werror -deprecation

// a version component in `since` that doesn't fit an Int
class Lib {
  @deprecated("use g", since = "20240131120000") def f = 1
  @deprecated("use g", since = "MyLib 1.99999999999") def h = 1
}

class Use {
  def g(l: Lib) = l.f + l.h
}
