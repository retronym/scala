package sp
class Foo10[@specialized(Int) T](val t: T) { def get: T = t }
