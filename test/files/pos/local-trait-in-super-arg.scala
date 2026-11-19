class P(a: Any)
class Outer {
  class I extends P({ trait LT { def kk = 1 }; class LC extends LT; new LC })
}
