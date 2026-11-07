package sp
class Foo8[@specialized(Int) T](val t: T) { def get: T = t }
