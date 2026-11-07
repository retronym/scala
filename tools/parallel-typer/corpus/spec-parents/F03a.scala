package sp
class Foo3[@specialized(Int) T](val t: T) { def get: T = t }
