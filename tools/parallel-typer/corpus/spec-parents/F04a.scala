package sp
class Foo4[@specialized(Int) T](val t: T) { def get: T = t }
