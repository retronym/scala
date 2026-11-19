//> using javacOpt -XDstringConcat=indy

// With -XDstringConcat=indy, javac uses StringConcatFactory.makeConcat (no constants in the recipe)
public class J {
  public static String cat(int n) { return "n=" + n + "!"; }
}
