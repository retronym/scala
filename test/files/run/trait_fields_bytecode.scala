// TODO: bytecode test

trait TFinal { final val bla: Int = 123 }

// bla should be final in C
class CFinal extends TFinal


trait TConst { final val C = "S" }
// there should be a C method in `T$class`!
class CConst extends TConst { println(C) }
