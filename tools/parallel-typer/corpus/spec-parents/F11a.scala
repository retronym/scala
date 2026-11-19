package sp
class Foo11[@specialized(Int) T](val t: T) { def get: T = t }
