package sp
class Foo6[@specialized(Int) T](val t: T) { def get: T = t }
