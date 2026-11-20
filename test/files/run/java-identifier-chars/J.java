//> using javacOpt -encoding UTF-8

// Java identifiers are made of the characters accepted by `Character.isJavaIdentifierStart/Part` (JLS 3.8),
// which include currency symbols and connector punctuation. The fields are not final, so that `Test` reads
// them at run time, by the name that javac gave them.
public class J {
  // currency symbols
  public int €uro = 1;
  public int a€b = 2;
  public int ¢ = 3;
  public int £() { return 4; }

  // connector punctuation other than `_`
  public int ‿x = 5;
  public int a‿b = 6;

  // supplementary characters: letters (Osmanya), a letter number (Gothic nine hundred) and a digit (mathematical bold zero)
  public int 𐒈𐒝 = 7;
  public int x𐒈 = 8;
  public int 𐍊 = 9;
  public int d𝟎 = 10;

  // written with unicode escapes
  public int \u20ACscaped = 11;
  public int \uD801\uDC88scaped = 12;

  // ignorable characters, here a zero width space, are not part of the name
  public int ig\u200Bnored = 13;

  public static class €Class { public int $ = 14; }

  // Scala encodes the two halves of a surrogate pair separately in its own names, so `Test` can't select the
  // members with supplementary characters above, wherever it reads `J` from. It can get at this class, though.
  public static class 𐒈Class { public int v = 15; }
  public 𐒈Class make() { return new 𐒈Class(); }
}
