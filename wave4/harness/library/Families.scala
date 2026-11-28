import scala.collection.{immutable => im, mutable => mu}
object Families {
  def uLt(a: Long, b: Long) = java.lang.Long.compareUnsigned(a, b) < 0
  lazy val all: Seq[Fam] = Seq(
    new LongMapFam, new AnyRefMapFam,
    new MutMapFam[CK]("mutable.OpenHashMap", new CK(_), () => mu.OpenHashMap.empty[CK, String], Nil),
    new MutMapFam[CK]("mutable.CollisionProofHashMap", new CK(_), () => mu.CollisionProofHashMap.empty[CK, String], Nil),
    new TrieMapFam("CK"), new TrieMapFam("spread"),
    new PatriciaFam[Int]("immutable.IntMap", _.toInt, () => im.IntMap.empty[String], (a, b) => Integer.compareUnsigned(a, b) < 0),
    new PatriciaFam[Long]("immutable.LongMap", identity, () => im.LongMap.empty[String], uLt),
    new ImmMapFam[CK]("immutable.TreeSeqMap[Insertion]", new CK(_), () => im.TreeSeqMap.empty[CK, String](im.TreeSeqMap.OrderBy.Insertion), "insertion", Nil),
    new ImmMapFam[CK]("immutable.TreeSeqMap[Modification]", new CK(_), () => im.TreeSeqMap.empty[CK, String](im.TreeSeqMap.OrderBy.Modification), "modification", Nil),
    new ImmMapFam[CK]("immutable.VectorMap", new CK(_), () => im.VectorMap.empty[CK, String], "insertion", Nil),
    new ImmMapFam[CK]("immutable.ListMap", new CK(_), () => im.ListMap.empty[CK, String], "insertion", Nil),
    new PQFam, new StackQueueFam, new ImmQueueFam
  ) ++ Extra.all
}
