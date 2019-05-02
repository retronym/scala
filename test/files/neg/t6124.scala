
trait T {
  def i: Int = 123_456_
  def j: Long = 123_456_L * 1000
  //def k = 123'455'

  def f = 3_14_E-2
  def e = 3_14E-_2
  def d = 3_14E-2_

  def p = 3.1_4_
  def q = 3.1_4_d
  def r = 3.1_4_dd
  def s = 3_.14
  def t = 3._14   // member selection

  def u = 0x_42
  def v = 0_x42

  def `error: malformed double precision floating point number` = 0_1.1
  def w = 0_1
  def x = 00_
  def y = 0_
  def z = 0

  def wtf = 0x    // see neg/literals.scala
}
