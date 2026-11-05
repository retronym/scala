public class Nul {
    public static final String Empty = "\0";
    public static final String Mixed = "a\0b\u0000c\\0";
    public static final String Octal = "\000\0";
    public static final char C0 = '\0';
    public static final char C1 = '\u0000';
    public static final String Block = """
        a\0b\
        c\u0000d
        """;
}
