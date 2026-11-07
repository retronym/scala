package sp
class Foo7[@specialized(Int) T](val t: T) { def get: T = t }
