package scala.collection.immutable.champ

private object Hashing {

  def elemHashCode(key: Any): Int = key.##

  def improve(hcode: Int): Int = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  def computeHash(key: Any): Int =
    improve(elemHashCode(key))
}