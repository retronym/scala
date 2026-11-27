trait N[-A]
trait Y extends N[Any]

// the right-hand type written out, not shared through an alias
trait D extends N[N[D with Y]]

// several traits in the loop
trait D1 extends N[H.ND]
trait D2 extends N[H.ND]
trait D3 extends N[H.ND]

// F-bounded
trait Fb[-T <: Fb[T]] extends N[H.NFb]
trait G extends Fb[G]

// mutually recursive
trait Ma extends N[H.NMb]
trait Mb extends N[H.NMa]

// two contravariant parameters: each turn of the loop asks four questions
trait N2[-A, -B]
trait Y2 extends N2[Any, Any]
trait E1 extends N2[H.NE, H.NE]
trait E2 extends N2[H.NE, H.NE]
trait E3 extends N2[H.NE, H.NE]
trait E4 extends N2[H.NE, H.NE]
trait E5 extends N2[H.NE, H.NE]
trait E6 extends N2[H.NE, H.NE]

object H {
  type ND  = N[D1 with D2 with D3 with Y]
  type NFb = N[G with Y]
  type NMa = N[Ma with Y]
  type NMb = N[Mb with Y]
  type E   = E1 with E2 with E3 with E4 with E5 with E6 with Y2
  type NE  = N2[E, E]
}

object Test {
  def d(p: (D, D)): (N[D with Y], N[D with Y]) = p
  def d123(p: (D1, D2, D3)): (H.ND, H.ND, H.ND) = p
  def d321(f: (H.ND, H.ND) => D3): (D2, D1) => H.ND = f
  def fb(p: (G, G)): (H.NFb, H.NFb) = p
  def mab(p: (Ma, Mb, Ma)): (H.NMb, H.NMa, H.NMb) = p
  def e(p: (E1, E6)): (H.NE, H.NE) = p
  def e2(p: N2[H.NE, H.NE]): N2[E3, E4] = p
}
