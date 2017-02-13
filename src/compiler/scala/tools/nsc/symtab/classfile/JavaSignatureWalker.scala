package scala.tools.nsc.symtab.classfile

import scala.reflect.internal.ClassfileConstants._

abstract class JavaSignatureWalker {
  def visitName(internalName: CharSequence): Unit
  def raiseError(msg: String): Unit

  def walk(sig: CharSequence): Unit = {
    def raiseError(msg: String): Unit = {
      this.raiseError(s"$msg (while parsing $sig)")
    }
    var index = 0
    val end = sig.length
    def accept(ch: Char) {
      assert(current() == ch, (current(), ch))
      index += 1
    }
    def skipSubName(isDelimiter: Char => Boolean): Unit = {
      val start = index
      while (!isDelimiter(current())) { index += 1 }
    }
    def appendSubName(builder: java.lang.StringBuilder, isDelimiter: Char => Boolean): CharSequence = {
      val start = index
      skipSubName(isDelimiter)
      builder.append(sig, start, index)
    }
    def current(): Char = {
      if (index < sig.length())
        sig.charAt(index)
      else '\0'
    }

    def parseMethodOrFieldType(): Unit = {
      val tag = current(); index += 1
      tag match {
        case OBJECT_TAG =>
          def parseTypeArgs(): Unit = {
            if (current() == '<') {
              accept('<')
              while (current() != '>') {
                current() match {
                  case '+' | '-' | '*' =>
                    index += 1
                  case _ =>
                }
                parseMethodOrFieldType()
              }
              accept('>')
            }
          }

          val internalName = new java.lang.StringBuilder
          appendSubName(internalName, c => c == ';' || c == '<')
          parseTypeArgs()
          while (current() == '.') {
            accept('.')
            internalName.append('$')
            appendSubName(internalName, c => c == ';' || c == '<' || c == '.')
            parseTypeArgs()
          }
          accept(';')
          visitName(internalName.toString)
        case ARRAY_TAG =>
          parseMethodOrFieldType()
        case '(' =>
          while (current() != ')') {
            parseMethodOrFieldType()
          }
          accept(')')
          parseMethodOrFieldType()
          while (current() == '^') {
            accept('^')
            parseMethodOrFieldType()
          }
        case TVAR_TAG =>
          skipSubName(';'.==)
          index += 1
        case BYTE_TAG | CHAR_TAG | DOUBLE_TAG | FLOAT_TAG | INT_TAG | LONG_TAG | SHORT_TAG | BOOL_TAG =>
        case VOID_TAG =>
        case _ =>
          raiseError("Unexpected tag: " + tag)
      }
    }

    def parseBounds(): Unit = {
      while (current() == ':') {
        index += 1
        if (current() != ':') // guard against empty class bound
          parseMethodOrFieldType()
      }
    }

    if (current() == '<') {
      index += 1
      val start = index
      while (current() != '>') {
        skipSubName(':'.==)
        parseBounds()
      }
      accept('>')
    }
    while (index < end) {
      parseMethodOrFieldType()
    }
  }

}
