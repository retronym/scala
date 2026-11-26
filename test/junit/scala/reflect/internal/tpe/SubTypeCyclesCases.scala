package scala.reflect.internal
package tpe

/** Hierarchies and subtype questions over them, picked from randomly generated ones. The answers are those of 2.13.16
 *  where it gives one: it has neither the cache of scala/bug#13119 nor its replacement, but does not finish every
 *  question. A pair is written `answer | lhs <:< rhs`.
 */
object SubTypeCyclesCases {
  final case class Family(definitions: String, pairs: String)

  val common: String =
    """|trait N[-A]
      |trait P[+A]
      |trait I[A]
      |trait N2[-A, -B]
      |trait Fn[-A, +B]
      |trait Y extends N[Any]
      |trait Z extends N2[Any, Any]
      |trait Fb[T <: Fb[T]] extends N[T]
      |trait G1 extends Fb[G1]
      |trait G2 extends Fb[G2] with Y
      |trait Ma[T <: Mb[T]] extends N[T]
      |trait Mb[T <: Ma[T]] extends P[T]
      |trait M1 extends Ma[M2]
      |trait M2 extends Mb[M1]
      |""".stripMargin

  val families: List[Family] = List(
    Family(
      """|object H {
        |  type A0 = N[D1]
        |  type A1 = N[D1 with G2]
        |  type A2 = N[D1 with Y]
        |}
        |trait D1 extends N[H.A2]""".stripMargin,
      """
        |true  | D1 <:< H.A2
        |true  | D1 <:< H.A2
        |true  | D1 <:< H.A2
        |true  | D1 <:< H.A1
        |true  | H.A1 <:< H.A1
        |true  | H.A0 <:< H.A2
        |true  | D1 <:< H.A0
        |false | D1 <:< N[H.A1]
        |false | H.A2 <:< H.A0
        |true  | D1 <:< N[H.A2]
        |true  | (D1, D1) <:< (H.A0, H.A2) // false in 2.13.18
        |true  | (H.A1, D1) <:< (H.A1, H.A0)
        |false | (H.A2, List[H.A2], List[List[D1]]) <:< (H.A0, List[H.A0], List[List[H.A2]])
        |false | (D1, D1) <:< (N[H.A1], H.A2)
        |false | N2[N[H.A1], H.A0] <:< N2[D1, D1]
        |true  | Fn[N[H.A2], D1] <:< Fn[D1, H.A2]
        |true  | (D1, I[D1]) <:< (H.A2, I[_ <: N[H.A2]])
        |true  | Fn[H.A1, D1] <:< Fn[D1, H.A2] // false in 2.13.18""".stripMargin),
    Family(
      """|object H {
        |  type A0 = N[D1]
        |  type A1 = N[D1 with G2]
        |}
        |trait D1 extends N[H.A1]""".stripMargin,
      """
        |false | N2[N2[D1, D1], D1] <:< H.A1
        |true  | D1 <:< H.A1
        |true  | D1 with Z <:< H.A0
        |true  | D1 with Y <:< H.A0
        |false | Fb[G1] <:< (D1, H.A1)
        |true  | D1 <:< H.A1
        |true  | D1 <:< H.A0
        |true  | D1 <:< H.A1
        |true  | D1 with Z <:< N2[H.A0, H.A1]
        |true  | D1 <:< H.A0
        |true  | (D1, D1 with Y) <:< (H.A1, H.A0)
        |false | (D1 with Z, N2[N2[D1, D1], D1]) <:< (N2[H.A0, H.A1], H.A1)
        |false | (N2[N2[D1, D1], D1], I[D1]) <:< (H.A1, I[_ <: H.A0])
        |true  | Fn[N2[H.A0, H.A1], D1 with Z] <:< Fn[D1 with Z, H.A0]
        |true  | (D1, List[D1], List[List[D1 with Z]]) <:< (H.A0, List[H.A0], List[List[H.A0]]) // false in 2.13.18
        |true  | (D1 with Z, D1 with Z) <:< (N2[H.A0, H.A1], N2[H.A0, H.A1])
        |true  | (D1, D1) <:< (H.A0, H.A0) // false in 2.13.18
        |true  | (D1, D1) <:< (H.A0, H.A1) // false in 2.13.18""".stripMargin),
    Family(
      """|object H {
        |  type A0 = N[D2 with D1 with Z]
        |  type A1 = N[D2 with Y]
        |}
        |trait D1 extends N[H.A0]
        |trait D2 extends N[N[D1 with D2]]""".stripMargin,
      """
        |false | D2 <:< H.A0 // no answer from 2.13.16
        |true  | D2 <:< H.A1 // no answer from 2.13.16
        |false | D2 <:< D1
        |true  | D2 <:< H.A1 // no answer from 2.13.16
        |false | H.A0 <:< N[H.A1]
        |true  | D2 <:< H.A1 // no answer from 2.13.16
        |true  | D1 with D2 with G2 <:< H.A1
        |false | N2[D2, D2 with Z] <:< D2 with D1 with G2
        |false | H.A0 <:< N[H.A0]
        |true  | D1 <:< N[N[D2 with D1]]
        |false | Fn[D1, D2] <:< Fn[D2, H.A0]
        |false | Fn[D1, H.A0] <:< Fn[D2, N[H.A1]]
        |false | (N2[D2, D2 with Z], List[N2[D2, D2 with Z]], List[List[D2]]) <:< (D2 with D1 with G2, List[D2 with D1 with G2], List[List[H.A1]])
        |true  | (D1 with D2 with G2, D2) <:< (H.A1, H.A1) // no answer from 2.13.16
        |false | N2[D2 with D1 with G2, N[H.A1]] <:< N2[N2[D2, D2 with Z], H.A0]
        |true  | Fn[H.A1, D2] <:< Fn[D2, H.A1] // no answer from 2.13.16
        |false | (D2, D2) <:< (H.A0, H.A1) // no answer from 2.13.16
        |false | (D2, D2) <:< (H.A1, H.A0) // no answer from 2.13.16""".stripMargin),
    Family(
      """|object H {
        |  type A0 = N[D2 with D1]
        |  type A1 = N[D2 with D1]
        |  type A2 = N[D2 with D1]
        |}
        |trait D1 extends N[H.A1]
        |trait D2 extends N[N[D2 with D3 with D1]]
        |trait D3 extends N[N[D1 with D2 with D3]] with P[Any]""".stripMargin,
      """
        |false | D3 <:< N2[D3 with D2 with D1 with Z, D1 with D3 with D2 with G2]
        |false | D3 <:< N[N2[D3 with D2 with D1 with G2, D2 with D3 with D1 with Y]]
        |false | D1 <:< N2[H.A0, H.A2]
        |true  | D1 with Y <:< H.A2 // no answer from 2.13.16
        |true  | D3 with Y <:< H.A0 // no answer from 2.13.16
        |false | H.A2 <:< N2[N[D3 with D1 with Z], N[D2 with D1 with D3 with Z]]
        |true  | D1 with D3 with D2 with G2 <:< N[D2 with Z] // no answer from 2.13.16
        |false | N[D1] <:< N2[D3, D1]
        |true  | D3 with D1 with D2 with Y <:< H.A2 // no answer from 2.13.16
        |false | D2 <:< H.A0 // no answer from 2.13.16
        |false | (D1, D1 with Y) <:< (N2[H.A0, H.A2], H.A2)
        |false | Fn[N[N2[D3 with D2 with D1 with G2, D2 with D3 with D1 with Y]], D2] <:< Fn[D3, H.A0]
        |false | (D2, D1 with Y) <:< (H.A0, H.A2) // no answer from 2.13.16
        |false | N2[N2[H.A0, H.A2], H.A2] <:< N2[D1, D1 with Y]
        |true  | (D1 with D3 with D2 with G2, List[D1 with D3 with D2 with G2], List[List[D1 with D3 with D2 with G2]]) <:< (N[D2 with Z], List[N[D2 with Z]], List[List[N[D2 with Z]]]) // no answer from 2.13.16
        |false | N2[N2[D3 with D2 with D1 with Z, D1 with D3 with D2 with G2], N2[N[D3 with D1 with Z], N[D2 with D1 with D3 with Z]]] <:< N2[D3, H.A2]
        |true  | (D3 with Y, List[D3 with Y], List[List[D3 with Y]]) <:< (H.A0, List[H.A0], List[List[H.A0]]) // no answer from 2.13.16
        |false | (D1, D1) <:< (N2[H.A0, H.A2], N2[H.A0, H.A2])""".stripMargin),
    Family(
      """|object H {
        |  type A0 = N[D1 with D3]
        |  type A1 = N[D1 with G2]
        |}
        |trait D1 extends N2[H.A1, H.A1]
        |trait D2 extends N[N[D3 with D2 with D1 with G2]] with G2
        |trait D3 extends N[N2[D3 with D2 with D1, D1 with Y]]""".stripMargin,
      """
        |true  | D1 with D3 <:< H.A1
        |true  | D3 <:< H.A1
        |true  | H.A0 <:< H.A0
        |true  | D2 with D3 with Y <:< N[H.A1]
        |true  | D2 with Z <:< N[H.A1]
        |false | D2 <:< N2[H.A1, N2[D2 with D1 with G2, D2 with D1 with D3 with Y]]
        |false | D1 <:< N[H.A0]
        |false | H.A1 <:< H.A0
        |true  | D2 <:< H.A0
        |false | D1 <:< H.A1
        |false | (D3, H.A1) <:< (H.A1, H.A0)
        |true  | (D3, List[D3], List[List[D1 with D3]]) <:< (H.A1, List[H.A1], List[List[H.A1]]) // false in 2.13.18
        |false | (D2, I[H.A0]) <:< (N2[H.A1, N2[D2 with D1 with G2, D2 with D1 with D3 with Y]], I[_ <: H.A0])
        |false | (D2, I[D2]) <:< (N2[H.A1, N2[D2 with D1 with G2, D2 with D1 with D3 with Y]], I[_ <: N2[H.A1, N2[D2 with D1 with G2, D2 with D1 with D3 with Y]]])
        |false | N2[H.A1, H.A1] <:< N2[D1, D3]
        |true  | (D2 with Z, List[D2 with Z], List[List[D1 with D3]]) <:< (N[H.A1], List[N[H.A1]], List[List[H.A1]])
        |true  | (D1 with D3, D1 with D3) <:< (H.A1, H.A1) // false in 2.13.18
        |true  | (D2, D2) <:< (H.A0, H.A0)""".stripMargin),
    Family(
      """|object H {
        |  type A0 = N[D1 with D2 with Z]
        |}
        |trait D1 extends N[H.A0] with P[Any]
        |trait D2 extends N[N[D1 with D2 with G2]] with Z""".stripMargin,
      """
        |true  | D2 <:< H.A0
        |false | D1 <:< N2[D2 with D1, H.A0]
        |true  | D1 <:< H.A0
        |true  | D2 <:< H.A0
        |true  | H.A0 <:< H.A0
        |true  | D1 with D2 <:< H.A0
        |true  | H.A0 <:< H.A0
        |true  | D2 <:< N[D2]
        |true  | D2 <:< H.A0
        |true  | H.A0 <:< H.A0
        |false | Fn[N2[D2 with D1, H.A0], D1] <:< Fn[D1, N2[D2 with D1, H.A0]]
        |false | Fn[H.A0, D1] <:< Fn[D2, N2[D2 with D1, H.A0]]
        |true  | (H.A0, D2) <:< (H.A0, H.A0)
        |true  | (D2, List[D2], List[List[D2]]) <:< (H.A0, List[H.A0], List[List[H.A0]]) // false in 2.13.18
        |true  | N2[H.A0, H.A0] <:< N2[H.A0, H.A0]
        |true  | (D2, D2) <:< (N[D2], N[D2])
        |true  | Fn[H.A0, D2] <:< Fn[D1, H.A0] // false in 2.13.18
        |true  | (D1, H.A0) <:< (H.A0, H.A0)""".stripMargin),
    Family(
      """|object H {
        |  type A0 = N[D2 with G2]
        |}
        |trait D1 extends N[H.A0]
        |trait D2 extends N[H.A0]""".stripMargin,
      """
        |true  | D2 <:< H.A0
        |true  | D2 <:< D2
        |true  | H.A0 <:< H.A0
        |true  | D2 with D1 <:< H.A0
        |true  | D2 <:< H.A0
        |true  | D1 <:< H.A0
        |true  | D1 <:< H.A0
        |true  | D2 with Y <:< N[I[_ >: D2]]
        |false | H.A0 <:< N[H.A0]
        |true  | H.A0 <:< H.A0
        |true  | (D2, D2) <:< (H.A0, D2)
        |false | (D2 with Y, List[D2 with Y], List[List[H.A0]]) <:< (N[I[_ >: D2]], List[N[I[_ >: D2]]], List[List[N[H.A0]]])
        |true  | (D2 with Y, List[D2 with Y], List[List[D2]]) <:< (N[I[_ >: D2]], List[N[I[_ >: D2]]], List[List[H.A0]])
        |true  | Fn[H.A0, D2] <:< Fn[D2 with D1, H.A0] // false in 2.13.18
        |true  | Fn[H.A0, D2 with Y] <:< Fn[D1, N[I[_ >: D2]]]
        |true  | N2[D2, H.A0] <:< N2[D2, H.A0]
        |true  | Fn[H.A0, H.A0] <:< Fn[D2 with D1, H.A0]
        |true  | (H.A0, I[D2]) <:< (H.A0, I[_ <: H.A0])""".stripMargin)
  )
}
