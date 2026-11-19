package sp
class Foo1[@specialized(Int) T](val t: T) { def get: T = t }
