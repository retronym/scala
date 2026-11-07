package sp
class Foo13[@specialized(Int) T](val t: T) { def get: T = t }
