This pull request holds a failing test and no fix.

In a joint compilation, a Java annotation whose argument is an operator expression is dropped. With

```java
public class J {
  @Deprecated(since = "1" + "2", forRemoval = false) public static int a() { return 1; }
  @Deprecated(since = "12") public static int b() { return 2; }
}
```

`J.a()` from Scala gets no deprecation warning, while `J.b()` does; compiled against the class file, both warn `(since 12)`. The same happens for any argument that is not a literal, a (qualified) name, a class literal, an array of those or a nested annotation: a negative number, a cast, a conditional, string concatenation. scalac reads few Java annotations from source (`@Deprecated`, and on annotation definitions `@Retention`, `@Target`, `@Repeatable`, `@Inherited`), so the visible loss is a deprecation warning, or in a `-Werror -deprecation` build its absence.

`JavaParsers.annotation` parses each element value with `elementValue`, which accepts only those forms; after the literal `"1"` the next token is `+`, so the argument list is abandoned and the annotation becomes `EmptyTree`. The comment there calls this intended until full support for Java constant expressions exists. Not a regression: 2.13.18 and 2.12 behave the same.

retronym/scala#288 (wave-one finding J5) fixes the same bail-out when the argument contains parentheses, which today makes the parser fail with "illegal start of type"; it still drops the annotation. A fix for this test would settle J5 as well and replace rather than extend retronym/scala#288.

Possible fix: parse an element value with the constant-expression parser that scala/scala#11290 added for field initializers (`constantExprOpt`), extended to stop at `,`, `)` and `}` and to accept class literals, and fold the result with the Java semantics of `JavaConstantFolder` (string conversion in `+`, casts, `char` arithmetic) when the annotation is typed, rather than with Scala's constant folder. An argument that is not a constant expression would still be dropped. The cost is that joint builds gain the deprecation warnings separate builds already have.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GJ3). Prepared with Claude.
