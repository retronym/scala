package sp
class Foo9[@specialized(Int) T](val t: T) { def get: T = t }
