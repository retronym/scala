// The names that lambdalift picks for lifted definitions must not clash with existing members.
class A {
  def f = { class B; new B }
  class B$1

  def local$1 = 1
  def k = { def local = 2; local }
}
