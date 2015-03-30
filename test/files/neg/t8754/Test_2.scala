object Test {
  def test(b: B[String]) = b.peer.getModel // ArrayIndexOutBoundsException in AsSeenFrom::correspondingTypeArg
}
