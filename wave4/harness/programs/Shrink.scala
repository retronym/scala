package w4p

import scala.collection.mutable.ArrayBuffer

/** Subexpression reduction in the generator's own syntax: every expression the generator nests is a bracketed group,
 *  a block is `{ s1; s2; e }` and a match is `{ case ...; case ... }`. Candidates for one line, larger first:
 *  replace a group by one of its directly nested groups, drop a statement of a block, drop a case arm. */
object Shrink {
  case class G(open: Int, close: Int, depth: Int) // indices of the brackets
  def groups(s: String): List[G] = {
    val out = ArrayBuffer[G](); val st = new java.util.ArrayDeque[Int]
    var i = 0
    while (i < s.length) {
      s.charAt(i) match {
        case '"' => // string literal (interpolations included): an atom
          if (s.startsWith("\"\"\"", i)) { val e = s.indexOf("\"\"\"", i + 3); i = if (e < 0) s.length else e + 2 }
          else { i += 1; while (i < s.length && s.charAt(i) != '"') { if (s.charAt(i) == '\\') i += 1; i += 1 } }
        case '\'' if i + 2 < s.length && (s.charAt(i + 2) == '\'' || s.charAt(i + 1) == '\\') =>
          i += 1; if (s.charAt(i) == '\\') i += 1; i += 1
        case '(' | '{' | '[' => st.push(i)
        case ')' | '}' | ']' => if (!st.isEmpty) { val o = st.pop(); out += G(o, i, st.size) }
        case _ =>
      }
      i += 1
    }
    out.toList
  }
  /** Top-level separators `;` inside group g (not inside nested groups or strings). */
  def topSemis(s: String, g: G, all: List[G]): List[Int] = {
    val inner = all.filter(c => c.open > g.open && c.close < g.close)
    val strs = stringSpans(s)
    (g.open + 1 until g.close).filter(i => s.charAt(i) == ';' && !inner.exists(c => c.open < i && i < c.close) && !strs.exists { case (a, b) => a < i && i < b }).toList
  }
  def stringSpans(s: String): List[(Int, Int)] = {
    val out = ArrayBuffer[(Int, Int)](); var i = 0
    while (i < s.length) {
      if (s.charAt(i) == '"') { val a = i; i += 1; while (i < s.length && s.charAt(i) != '"') { if (s.charAt(i) == '\\') i += 1; i += 1 }; out += ((a, i)) }
      i += 1
    }
    out.toList
  }
  /** Reduced variants of one line, roughly largest reduction first. */
  def candidates(s: String): Iterator[String] = {
    val gs = groups(s).sortBy(g => (g.depth, -(g.close - g.open)))
    gs.iterator.flatMap { g =>
      val kids = gs.filter(c => c.open > g.open && c.close < g.close && c.depth == g.depth + 1 && s.charAt(c.open) != '[')
      val body = s.substring(g.open + 1, g.close)
      val hoist = if (s.charAt(g.open) == '[') Nil else kids.sortBy(c => c.close - c.open).map(c => s.substring(0, g.open) + "(" + s.substring(c.open + 1, c.close) + ")" + s.substring(g.close + 1))
      val semis = if (s.charAt(g.open) == '{') topSemis(s, g, gs) else Nil
      // pieces between top-level semicolons; drop one (not the last of a block, which is its value; any case arm but the last)
      val bounds = (g.open :: semis) zip (semis :+ g.close)
      val drops = if (bounds.length < 2) Nil else bounds.init.map { case (a, b) => s.substring(0, a + 1) + s.substring(b + 1) } ++
        (if (body.trim.startsWith("case")) { val (a, b) = bounds.last; List(s.substring(0, a) + " " + s.substring(b)) } else Nil)
      // a match: keep one arm's body
      hoist.iterator ++ drops.iterator
    }.filter(_ != s)
  }
}
