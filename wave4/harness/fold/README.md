# fold: metamorphic fuzzer for primitive expression semantics

`fold.py` generates well-typed expression trees over Byte, Short, Char, Int, Long, Float, Double, Boolean and String and checks that different spellings of one tree agree at run time. No second compiler is needed.

## What it generates

Arithmetic, bitwise operators and shifts (Int and Long counts, counts beyond the width), comparisons, unary operators, every numeric conversion, `==`/`!=`/`equals`/`##`/`hashCode`/`toString` across types, string concatenation on either side, `if` with weakly conforming branches, and `match` on literals. Leaves come from pools of boundary values (MinValue, -1, 2^24+1, NaN, -0.0, infinities, values outside the Int and Long range).

`--evolved` adds Unit and Null operands, `null.asInstanceOf[primitive]`, blocks, `try`, `while`, a branch that throws, type-pattern and literal-pattern matches on `Any`, `isInstanceOf`, and hex, binary, underscore and exponent literal syntax.

## Oracle

Each tree is printed in up to 22 forms, each compiled with no optimizer flags and with `-opt:inline:** -opt:local`; all lines for one tree must be equal. Values are printed with a type tag and floats as raw bits; an exception prints its class.

| form | spelling |
|---|---|
| A | literals inline (the typer folds) |
| B, C, L | literals read from a `var`, through `@noinline def id[T]`, from local vals |
| F, G | `final val` and literal-typed vals in another object; in a separately compiled unit |
| V, W | conversions written as `asInstanceOf` |
| X, Y, Z | operands of `==`, `##`, `equals`, `hashCode`, `toString`, `String +` as `Any` or `java.lang` boxes |
| P, H, T, R, M, E, O | each leaf through a default argument, a method parameter, a tuple, a captured var, a value class, an array, `Option`/`Some`/`List` |
| I | same-type arithmetic and comparisons through `Numeric`/`Integral`/`Fractional`/`Ordering` |
| Q | binary operators as compound assignment |
| D, N | the whole expression as a default argument; as a Java annotation argument read back by reflection |

A form the compiler rejects is dropped for that tree and the rejection is reported by message. Failures are grouped by signature (the partition of forms and the kinds of value). The first case of each signature is shrunk by hoisting same-typed descendants and replacing subtrees with leaves, all candidates of one step in one compilation.

## Run

```
python3 fold.py --work <scratch dir> --seed 1 --n 300 --batches 8 --budget 150            # round one forms
python3 fold.py --work <scratch dir> --seed 1 --n 120 --batches 5 --budget 100 --evolved  # all forms
python3 fold.py ... --build base        # the unfixed base
python3 fold.py ... --jacoco cov.exec   # attach JaCoCo to the compiler JVM
```

`--budget` stops starting new batches after that many seconds; shrinking runs after it. Measured rate on the laptop: about 370 trees per minute with the ten round-one forms at depth 5 (3,700 form pairs per minute across both optimizer settings), about 150 trees per minute with all forms.

## Known limits

It shells out to `bin/scalac` per batch rather than reusing a JVM. A compiler crash loses the whole batch, so the generator avoids the one crash it found (a match whose scrutinee folds to Nothing).
