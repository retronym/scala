scalac's Java parser rejects `enum E { , }` and `enum E { ,; ... }` with "identifier expected but `,` found", so a mixed compilation containing such a file fails although javac accepts it. JLS 8.9.1 gives the enum body as `{ [EnumConstantList] [,] [EnumBodyDeclarations] }`, so a comma with no constants before it is legal. Only generated code is likely to contain it.

`JavaParsers.enumDecl` parses a constant whenever the body does not start with `}`, `;` or end of file, and looks for a comma only after a constant. The parser now skips a leading comma when `}` or `;` follows it.

Long-standing (2.12 is the same). Scala 3 rejects these files too. No ticket found.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GJ4). Prepared with Claude.
