package p

class VG[T](val t: T) extends AnyVal

object Sigs {
  // These need a generic signature; the array element types must erase as in the descriptor.
  def arrVG(x: Array[VG[String]]): Array[VG[String]] = x
  def arrNothing[T](x: Array[Nothing], t: T): Array[Null] = new Array[Null](1)
}
