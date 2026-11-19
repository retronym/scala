object Test {
  val ok          = <a b="&#65;&#x41;&#x10FFFF;">&#65;&#x41;&#x10FFFF;</a>

  val tooBig      = <a>&#x110000;</a>
  val wrapsHex    = <a>&#x100000041;</a>
  val wrapsDec    = <a>&#4294967361;</a>
  val noDigits    = <a>&#;</a>
  val noHexDigits = <a>&#x;</a>
  val hexInDec    = <a>&#1af;</a>
  val notDigits   = <a>&#xyz;</a>

  val attrTooBig  = <a b="&#x110000;"/>
  val attrWraps   = <a b="&#x100000041;"/>
  val attrEmpty   = <a b="&#;"/>
}
