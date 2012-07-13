package foo {
  class Base[M](i: Int)

  class Derived()(implicit i: Int) extends Base({println(i); 0})

  // okay
  object O {
    class ParametricMessage[M: Manifest](msg: M)
    class ParametricMessage1[M: Manifest](msg: M, p1: Class[_]) extends ParametricMessage(msg)
  }

  class PMTopLevel[M: Manifest](msg: M)
  class PM1TopLevel[M: Manifest](msg: M, p1: Class[_]) extends PMTopLevel(msg)

  object `package` {
    // implicit def evidence$2: Manifest[Any] = ??? // clashes!
  }
}

/*


 Within the type completer for `Derived`.

 classSig
   templateSig
     parentTypes  -  cscope = context.outer.makeNewScope // package <empty>
      typePrimaryConstrBody  -  namer.enterValueParams(vparamss)

       ...
         typedArg  - fun = new [M]<empty>.this.Base[M][M].<init>
                     args = List({println(i);0})
                     i.sym.owner = package <empty>

           ...
              i.info
                Namers#complete
                  ...
                     Namers#validate
                        i.owner.isPackageClass = true // KABOOM

 Enclosing Derived in an object masks the problem.

 Adding an explicit type argument to the `Base` constructor invocation masks the problem.

 */
