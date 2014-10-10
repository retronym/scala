import java.util.Date

trait TDate 

trait TT[A1,T1]

trait TTFactory[F,G] {
  def create(f: F) : TT[F,G]
  def sample: F
}

object Test1 {

  // If the c1 is declared before c2, it compiles fine
  implicit def c2(s: Date) = c1.create(s)  

  val c1 = new TTFactory[Date,TDate] {
    def create(v: Date): TT[Date,TDate] = sys.error("")
    def sample = new Date
  }
}


object Test2 {

  // If the c1 is declared before c2, it compiles fine
  implicit def c2(s: Date) = c1.create(s)  

  val c1 = {
    class A extends TTFactory[Date,TDate] {
      def create(v: Date): TT[Date,TDate] = sys.error("")
      def sample = new Date
    }
    new A
  }
}