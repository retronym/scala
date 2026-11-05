package ann

object Old {
  @deprecated("use New.now", "1.0") def old: Int = 1
  @deprecated("use New.Thing", "1.0") class Thing
}
