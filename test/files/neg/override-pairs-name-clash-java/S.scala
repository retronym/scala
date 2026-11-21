// Members that are Java-defined are left out of the pairs RefChecks records, as it cooks their raw types.
trait K1 { def generic(l: java.util.List[Int]): Unit }
abstract class JavaClash extends J with K1

trait K2 { def raw(l: java.util.List[String]): Unit }
abstract class RawParent extends J with K2
