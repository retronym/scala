When a Java record with a variable arity component is compiled together with Scala, the component's accessor has the repeated-parameter type instead of an array type. With `record Command(String name, String... args)`, `c.args.toList`, `c.args.map(...)`, `c.args.mkString` and `List(c.args)` are all rejected in joint compilation ("value toList is not a member of String*"), while the same code compiles against the class file, where `args()` returns `String[]`. JLS 8.10.3 gives the accessor the declared type of the component, which for a variable arity component is the array type.

`JavaParsers.recordDecl` generates the accessors from the record header's parameters and gave each one a copy of the parameter's type tree. For a variable arity component that tree is the `<repeated...>` type built by `formalParam`, which is right for the canonical constructor but not for the accessor. The accessor now gets `Array[T]` in that case; the canonical constructor keeps `T*`.

Long-standing, since records were supported by the Java parser in 2.13.7. No ticket found.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GJ2). Prepared with Claude.
