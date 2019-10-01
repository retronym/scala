import annotation.switch
class Switches {
  val cond = true
  def two = ("foo" : @switch) match { case "foo" => case "bar" => }
  def guard = ("foo" : @switch) match { case "z" => case "y" => case x if cond => }
  def colli = ("foo" : @switch) match { case "DB" => case "Ca" => }
}