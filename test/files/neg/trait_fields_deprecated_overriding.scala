package scala

trait DeprecatedOverriding {
  @deprecatedOverriding val x = 1
}

class C extends DeprecatedOverriding {
  override val x = 2
}
