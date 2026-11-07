package sp
class Foo15[@specialized(Int) T](val t: T) { def get: T = t }
