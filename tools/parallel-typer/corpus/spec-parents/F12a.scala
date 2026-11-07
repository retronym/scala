package sp
class Foo12[@specialized(Int) T](val t: T) { def get: T = t }
