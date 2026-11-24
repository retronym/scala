object Test extends App {
  val j = new J
  val values = List(
    j.`€uro`, j.`a€b`, j.`¢`, j.`£`(),
    j.`‿x`, j.`a‿b`,
    j.`€scaped`,
    j.ignored,
    new J.`€Class`().$,
    j.make().v
  )
  assert(values == List(1, 2, 3, 4, 5, 6, 11, 13, 14, 15), values)
}
