object Test {
  ((false || (throw null)) && false) // crash
  ((true || (throw null)) && false) // crash
  ((((throw null): Boolean) || true) && false)
  ((((throw null): Boolean) || false) && false)

  ((false && (throw null)) || false) // crash
  ((true && (throw null)) || false) // crash
  ((((throw null): Boolean) && true) || false)
  ((((throw null): Boolean) && false) || false)
}
