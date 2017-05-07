package test;

// This should stay in sync with Annotated_0 to test annotations defined in an earlier compilation run

@NoArgs_0
@Simple_0(_byte = 1, _char = '2', _short = Simple_0.THREE, _int = 4, _long = 5, _float = 6.7f, _double = 8.9, _string = "ten", _class = Object.class)
@Nested_0(
    inner = @Nested_0.Inner("turkey")
)
@Array_0.Repeated({
        @Array_0({8, 6, 7, 5, 3, 0, 9}),
        @Array_0(6)
})
@Enum_0(choice = Enum_0.Enum.ONE)
public class Annotated_1 {}

