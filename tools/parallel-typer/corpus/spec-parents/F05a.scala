package sp
class Foo5[@specialized(Int) T](val t: T) { def get: T = t }
