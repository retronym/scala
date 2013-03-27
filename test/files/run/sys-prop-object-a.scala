object Test extends App {
  def props() = scala.tools.reflect.WrappedProperties.AccessControl.systemProperties.toList
  val props1 = props()
  System.getProperties.put("foo", new {})
  val props2 = props() // non string properties should be ignored.
  assert(props2 == props2, (props1, props2))
}
