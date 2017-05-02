/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package cmd

import scala.annotation.tailrec

/** A simple (overly so) command line parser.
 *  !!! This needs a thorough test suite to make sure quoting is
 *  done correctly and portably.
 */
object CommandLineParser {
  // splits a string into a quoted prefix and the rest of the string,
  // taking escaping into account (using \)
  // `"abc"def` will match as `DoubleQuoted(abc, def)`
  private class QuotedExtractor(quote: Char) {
    def unapply(in: String): Option[(String, String)] = {
      val del = quote.toString
      if (in startsWith del) {
        var escaped = false
        val (quoted, next) = (in substring 1) span {
          case `quote` if !escaped => false
          case '\\'    if !escaped => escaped = true; true
          case _                   => escaped = false; true
        }
        // the only way to get out of the above loop is with an empty next or !escaped
        // require(next.isEmpty || !escaped)
        if (next startsWith del) Some((quoted, next substring 1))
        else None
      } else None
    }
  }
  private object DoubleQuoted extends QuotedExtractor('"')
  private object SingleQuoted extends QuotedExtractor('\'')
  object Word {
    private val regex = """(\S+)""".r
    def unapply(s: CharSequence): Option[(String, CharSequence)] = {
      regex.findPrefixOf(s) match {
        case Some(prefix) => Some(prefix, s.subSequence(prefix.length, s.length()))
        case None => None
      }
    }
  }

  // parse `in` for an argument, return it and the remainder of the input (or an error message)
  // (argument may be in single/double quotes, taking escaping into account, quotes are stripped)
  private def argument(in: CharSequence): Either[String, (String, CharSequence)] = in match {
    case DoubleQuoted(arg, rest) => Right((arg, rest))
    case SingleQuoted(arg, rest) => Right((arg, rest))
    case Word(arg, rest)         => Right((arg, rest))
    case _                       => Left(s"Illegal argument: $in")
  }

  // parse a list of whitespace-separated arguments (ignoring whitespace in quoted arguments)
  @tailrec private def commandLine(in: CharSequence, accum: List[String] = Nil): Either[String, (List[String], String)] = {
    val trimmed = trim(in)
    if (trimmed.length() == 0) Right((accum.reverse, ""))
    else argument(trimmed) match {
      case Right((arg, next)) =>
        span(next, Character.isWhitespace) match {
          case(ws, rest) if ws.length() == 0 && rest.length() != 0 =>
            Left("Arguments should be separated by whitespace.") // TODO: can this happen?
          case(ws, rest)                  => commandLine(rest, arg.toString :: accum)
        }
      case Left(msg) => Left(msg)
    }
  }

  private def trim(cs: CharSequence): CharSequence = {
    def isSpace(c: Char) = c < '\u0020'
    var end = cs.length
    var start = 0
    while ((start < end) && isSpace(cs.charAt(start)))
      start += 1
    while ((start < end) && isSpace(cs.charAt(end - 1)))
      end -= 1
    if ((start > 0) || (end < cs.length)) cs.subSequence(start, end)
    else cs
  }
  private def span(cs: CharSequence, f: Char => Boolean): (CharSequence, CharSequence) = {
    val i = prefixLength(cs, f)
    (cs.subSequence(0, i), cs.subSequence(i, cs.length()))
  }
  private def prefixLength(cs: CharSequence, f: Char => Boolean): Int = {
    (0 until cs.length()).prefixLength(i => f(cs.charAt(i)))
  }

  class ParseException(msg: String) extends RuntimeException(msg)

  def tokenize(line: String): List[String] = tokenize(line, x => throw new ParseException(x))
  def tokenize(line: String, errorFn: String => Unit): List[String] = {
    commandLine(line) match {
      case Right((args, _)) => args
      case Left(msg)        => errorFn(msg) ; Nil
    }
  }
}
