package sp
class Foo2[@specialized(Int) T](val t: T) { def get: T = t }
