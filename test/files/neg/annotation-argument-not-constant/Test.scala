object K {
  final val One = 1
  final val Hello = "hello"
  def three(): 3 = { println("three"); 3 }
  def id[T <: Singleton](t: T): T = t
}

@Ann(z = { println("side effect"); false }) class Block1
@Ann(i = { val u = 1 / 0; 3 }) class Block2
@Ann(i = K.three()) class Call1
@Ann(i = K.id(5)) class Call2
@Ann(i = K.three() + 1) class Call3

class C(i: Int) extends scala.annotation.ConstantAnnotation
@C({ println("side effect"); 3 }) class Block3

// constant expressions
@Ann(z = false, i = 3, s = "a") class Ok1
@Ann(i = K.One + 2, s = K.Hello + " world") class Ok2
@Ann(i = -K.One, s = "a" + "b") class Ok3
@Ann(i = K.One.toInt) class Ok4
@C(K.One * 2) class Ok5
