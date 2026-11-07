package sp
class Foo14[@specialized(Int) T](val t: T) { def get: T = t }
