// A compound type prints its singleton and abstract type parents:
// only a redundant reference to Object is dropped.
object Test {
  object a { val x: AnyRef = "" }
  object b { val x: AnyRef = "" }
  val y: AnyRef = ""
  type Alias = AnyRef

  def singletons(i: a.x.type with b.x.type): Int = i
  def singletonAndClass(i: y.type with Serializable): Int = i
  def classAndSingleton(i: Serializable with y.type): Int = i
  def singletonAndObject(i: y.type with AnyRef): Int = i
  def refined(i: y.type { def f: Int }): Int = i
  def abstractAndClass[T <: AnyRef](i: T with Serializable): Int = i
  def abstracts[T <: AnyRef, U <: AnyRef](i: T with U): Int = i

  // references to Object are still dropped
  def objectAndClass(i: AnyRef with Serializable): Int = i
  def aliasAndClass(i: Alias with Serializable): Int = i
  def objects(i: AnyRef with Object { def f: Int }): Int = i
}
