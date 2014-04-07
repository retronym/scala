trait Tables {
  type Column[T]
  type HList
  type HNil <: HList
  type HCons[+Value, +Tail <: HList] <: HList
  type :: [+Value, +Tail <: HList] = HCons[Value, Tail]
  
  trait ShapeLevel
  object ShapeLevel{
    trait Nested extends ShapeLevel
    trait Flat extends Nested
    trait Columns extends Flat
  }
  
  abstract class Shape[Level <: ShapeLevel, -Mixed_, Unpacked_, Packed_]
  abstract class HListShape[Level <: ShapeLevel, M <: HList, U <: HList, P <: HList] extends Shape[Level, M, U, P]
    
  trait LowPriorityImplicits2 {
    implicit def hnilShape[Level <: ShapeLevel]: HListShape[Level, HNil, HNil, HNil] = ???
    implicit def hconsShape[Level <: ShapeLevel, L1 <: Level, L2 <: Level, M1, M2 <: HList, U1, U2 <: HList, P1, P2 <: HList](implicit s1: Shape[L1, M1, U1, P1], s2: HListShape[L2, M2, U2, P2]) : HListShape[Level, M1 :: M2, U1 :: U2, P1 :: P2] = ???
  }
  trait LowPriorityImplicits1 extends LowPriorityImplicits2 {
    // This implicit is redundant (hconsShape would be sufficient) but it serves to
    // limit the depth of implicit search through `hconsShape`, which has
    // exponential performance (albeit at a low constant factor) shows up with hlists
    // in the order of 100 elements. See SI-8478.
    def hconsShape8[Level <: ShapeLevel, 
         L1 <: Level, L2 <: Level, L3 <: Level, L4 <: Level, L5 <: Level, L6 <: Level, L7 <: Level, L8 <: Level, 
         M1, M2, M3, M4, M5, M6, M7, M8 <: HList, 
         U1, U2, U3, U4, U5, U6, U7, U8 <: HList, 
         P1, P2, P3, P4, P5, P6, P7, P8 <: HList](
          implicit
          s1: Shape[L1, M1, U1, P1], 
          s2: Shape[L2, M2, U2, P2], 
          s3: Shape[L3, M3, U3, P3], 
          s4: Shape[L4, M4, U4, P4], 
          s5: Shape[L5, M5, U5, P5], 
          s6: Shape[L6, M6, U6, P6], 
          s7: Shape[L7, M7, U7, P7], 
          s8: HListShape[L8, M8, U8, P8]
     ) : HListShape[Level, M1 :: M2 :: M3 :: M4 :: M5 :: M6 :: M7 :: M8,
                           U1 :: U2 :: U3 :: U4 :: U5 :: U6 :: U7 :: U8,
                           P1 :: P2 :: P3 :: P4 :: P5 :: P6 :: P7 :: P8] = ???
  }
  object Implicits extends LowPriorityImplicits1 {
    implicit def primitiveShape[Level <: ShapeLevel]: Shape[Level, String, String, Column[String]] = ???    
  }
 
  import Implicits._
   
  
  type HList10[T <: HList] = HCons[String,HCons[String,HCons[String,HCons[String,HCons[String,HCons[String,HCons[String,HCons[String,HCons[String,HCons[String,T]]]]]]]]]]
  type HList100[T <: HList] = HList10[HList10[HList10[HList10[HList10[HList10[HList10[HList10[HList10[HList10[T]]]]]]]]]]
  type HList1000[T <: HList] = HList100[HList100[HList100[HList100[HList100[HList100[HList100[HList100[HList100[HList100[T]]]]]]]]]]
  
  type JobRow = HList1000[HNil]
  
  implicitly[Shape[_ <: ShapeLevel.Flat, JobRow, JobRow, _]] 
}
