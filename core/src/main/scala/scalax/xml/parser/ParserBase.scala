/*
* Copyright (c) 2017 Lucas Satabin
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package scalax.xml
package parser

import scala.io.Source

import scala.collection.immutable.{
  Queue,
  VectorBuilder
}

import scala.annotation.{
  tailrec,
  switch
}

abstract class ParserBase(private var inputs: Queue[Source], protected var is11: Boolean) extends AutoCloseable {

  def this(input: Source, is11: Boolean) =
    this(Queue(input), is11)

  import XmlUtils._

  /** Feeds this parser with more content from the given source. */
  def feed(src: Source): Unit =
    inputs = inputs.enqueue(src)

  protected def fail(prod: String, msg: String): Nothing =
    fail(XmlSyntax(prod), msg)

  protected def fail(error: XmlError, msg: String): Nothing =
    throw XmlException(new LineColumnPosition(line, column), error, msg)

  private def currentInput = inputs.headOption

  protected def line = currentInput.fold(0)(_.RelaxedPositioner.cline)

  protected def column = currentInput.fold(0)(_.RelaxedPositioner.ccol)

  def close(): Unit =
    inputs.foreach(_.close())

  private var buffer = Option.empty[Char]

  @tailrec
  final protected def readInput(): Unit =
    inputs.dequeueOption match {
      case Some((input, rest)) =>
        if (input.hasNext) {
          val c = input.next()
          if (isValid(c))
            buffer = Some(c)
          else
            fail("2", "forbidden character")
        } else {
          input.close()
          inputs = rest
          readInput()
        }
      case None =>
        buffer = None
    }

  protected def peekChar(): Option[Char] =
    buffer.orElse {
      readInput()
      buffer
    }

  protected def nextCharOpt(): Option[Char] = {
    val c = peekChar()
    buffer = None
    c
  }

  /** Consumes the next character in source and returns it. */
  protected def nextChar(): Char =
    nextCharOpt().getOrElse(fail("1", "Unexpected end of input"))

  protected def accept(c: Char, error: String, msg: String): Unit = peekChar() match {
    case Some(n) if n == c =>
      nextChar()
    case _ =>
      fail(error, msg)
  }

  protected def assert(p: Char => Boolean, error: String, msg: String): Char = peekChar() match {
    case Some(c) if p(c) =>
      nextChar()
    case _ =>
      fail(error, msg)
  }

  protected def untilChar(p: Char => Boolean, sb: StringBuilder): StringBuilder = {
    @tailrec
    def loop(sb: StringBuilder): StringBuilder =
      peekChar() match {
        case Some(c) if !p(c) =>
          nextChar()
          loop(sb.append(c))
        case _ =>
          sb
      }
    loop(sb)
  }

  protected def read(s: String): Int = {
    @tailrec
    def loop(idx: Int, acc: Int): Int =
      if (idx >= s.length)
        acc
      else peekChar() match {
        case Some(c) if c == s.charAt(idx) =>
          nextChar()
          loop(idx + 1, acc + 1)
        case _ =>
          acc
      }
    loop(0, 0)
  }

  protected def isValid(c: Int): Boolean =
    if (is11)
      // [#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
      (0x1 <= c && c <= 0xd7ff) || (0xe000 <= c && c <= 0xfffd) || (0x10000 <= c && c <= 0x10ffff)
    else
      // #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
      c == 0x9 || c == 0xa || c == 0xd || (0x20 <= c && c <= 0xd7ff) || (0xe000 <= c && c <= 0xfffd) || (0x10000 <= c && c <= 0x10ffff)

  protected def space(): Boolean = {
    @tailrec
    def loop(acc: Boolean): Boolean =
      peekChar() match {
        case Some(c) if isXmlWhitespace(c) =>
          nextChar()
          loop(true)
        case _ =>
          acc
      }
    loop(false)
  }

  protected def space1(prod: String, msg: String): Unit = {
    assert(isXmlWhitespace(_), prod, msg)
    space()
  }

  protected def isNCNameStart(c: Char): Boolean = {
    import java.lang.Character._

    getType(c).toByte match {
      case LOWERCASE_LETTER |
        UPPERCASE_LETTER | OTHER_LETTER |
        TITLECASE_LETTER | LETTER_NUMBER => true
      case _ => c == '_'
    }
  }

  protected def isNCNameChar(c: Char): Boolean = {
    import java.lang.Character._
    // The constants represent groups Mc, Me, Mn, Lm, and Nd.

    isNCNameStart(c) || (getType(c).toByte match {
      case COMBINING_SPACING_MARK |
        ENCLOSING_MARK | NON_SPACING_MARK |
        MODIFIER_LETTER | DECIMAL_DIGIT_NUMBER => true
      case _ => ".-·".contains(c)
    })
  }

  protected def readNCName(): String = {
    val c = nextChar()
    if (isNCNameStart(c)) {
      val sb = new StringBuilder
      untilChar(c => !isNCNameChar(c), sb.append(c))
      sb.toString
    } else {
      fail("5", f"character '$c' cannot start a NCName")
    }
  }

  protected def readQName(): QName = {
    val part1 = readNCName()
    peekChar() match {
      case Some(':') =>
        nextChar()
        val part2 = readNCName()
        QName(Some(part1), part2, None)
      case _ =>
        QName(None, part1, None)
    }
  }

  /** Reads the nextChar markup token */
  protected def readMarkupToken(): MarkupToken = {
    accept('<', "43", "expected token start")
    val l = line
    val c = column
    peekChar() match {
      case Some('/') =>
        nextChar()
        val qname = readQName()
        space()
        accept('>', "42", "missing '>' at the end of closing tag")
        EndToken(qname, l, c)
      case Some('?') =>
        nextChar()
        PIToken(readNCName(), l, c)
      case Some('!') =>
        nextChar()
        peekChar() match {
          case Some('-') =>
            nextChar()
            skipComment(l, c)
          case Some('[') =>
            nextChar()
            readSection(l, c)
          case _ =>
            DeclToken(readNCName(), l, c)
        }
      case _ =>
        StartToken(readQName(), l, c)
    }
  }

  /** We have read '<![' so far */
  private def readSection(l: Int, c: Int): SectionToken = {
    space()
    val n =
      peekChar() match {
        case Some('%') =>
          nextChar();
          val n = readNCName()
          accept(';', "69", "expected ';' at the end of a parameter entity reference")
          Right(n)
        case _ =>
          Left(readNCName())
      }
    space()
    accept('[', "19", "'[' expected")
    SectionToken(n, l, c)
  }

  /** We have read '<!-' so far */
  protected def skipComment(l: Int, c: Int): CommentToken = {
    accept('-', "15", "second dash missing to open comment")
    def loop(): Unit = (nextChar(): @switch) match {
      case '-' =>
        (nextChar(): @switch) match {
          case '-' =>
            accept('>', "15", "'--' is not inside comments")
          case _ =>
            loop()
        }
      case _ =>
        loop()
    }
    loop()
    CommentToken(l, c)
  }

  /** We have just read the PI target */
  protected def readPIBody(): String = {
    space()
    @tailrec
    def loop(sb: StringBuilder): String = {
      val sb1 = untilChar(c => c == '?', sb)
      // read potential ?
      nextCharOpt()
      peekChar() match {
        case Some('>') =>
          nextChar()
          sb.toString
        case Some(_) =>
          loop(sb.append('?'))
        case None =>
          fail("16", "unexpected end of input")
      }
    }
    loop(new StringBuilder)
  }

  @tailrec
  final protected def readAttributeValue(delim: Option[Char], current: StringBuilder, builder: VectorBuilder[XmlTexty], l: Int, c: Int): Seq[XmlTexty] = {
    val delimiters = delim.fold(valueDelimiters)(valueDelimiters + _)
    untilChar(delimiters.contains(_), current)
    nextCharOpt() match {
      case `delim` =>
        if (!current.isEmpty)
          builder += XmlString(current.toString, false)(l, c)
        builder.result()
      case None => fail("10", "unexpected end of input")
      case Some('\r') =>
        peekChar() match {
          case Some('\n') =>
            readAttributeValue(delim, current.append(nextChar()), builder, l, c)
          case _ =>
            readAttributeValue(delim, current.append(' '), builder, l, c)
        }
      case Some(c) if isXmlWhitespace(c) =>
        readAttributeValue(delim, current.append(' '), builder, l, c)
      case Some('&') =>
        val l = line
        val c = column
        builder += XmlString(current.toString, false)(l, c)
        peekChar() match {
          case Some('#') =>
            nextChar()
            val n = readCharRef()
            builder += XmlCharRef(n)(l, c)
            readAttributeValue(delim, new StringBuilder, builder, line, column)
          case _ =>
            val s = readNamedEntity()
            builder += XmlEntityRef(s)(l, c)
            readAttributeValue(delim, new StringBuilder, builder, line, column)
        }
      case Some(c) =>
        fail("10", f"unexpected character '$c'")
    }
  }

  protected def readNamedEntity(): String = {
    val name = readNCName()
    accept(';', "68", "named entity must end with a semicolon")
    name
  }

  /** We read '&#' so far */
  protected def readCharRef(): Int = {
    def postlude(n: Int) =
      (nextChar(): @switch) match {
        case ';' =>
          if (isValid(n))
            n
          else
            fail("2", "invalid character")
        case _ =>
          fail("66", "character reference must end with a semicolon")
      }
    peekChar() match {
      case Some('x') =>
        nextChar()
        val n = readNum(16)
        postlude(n)
      case _ =>
        val n = readNum(10)
        postlude(n)
    }
  }

  private def readNum(base: Int): Int = {
    object Digit {
      def unapply(c: Option[Char]): Option[Int] =
        c match {
          case Some(c) =>
            if ((base == 10 || base == 16) && '0' <= c && c <= '9')
              Some(c - '0')
            else if (base == 16 && 'a' <= c && c <= 'f')
              Some(c - 'a' + 10)
            else if (base == 16 && 'A' <= c && c <= 'F')
              Some(c - 'A' + 10)
            else
              None
          case None =>
            None
        }
    }

    @tailrec
    def rest(acc: Int): Int =
      peekChar() match {
        case Digit(d) =>
          nextChar()
          rest(acc * base + d)
        case _ =>
          acc
      }

    nextCharOpt() match {
      case Digit(d) => rest(d)
      case _        => fail("66", "bad first character reference digit")
    }
  }

  protected def readExternalID(s: Boolean, optionalPublicSystem: Boolean): Option[ExternalId] = {
    peekChar() match {
      case Some(c) if isNCNameStart(c) =>
        if (s) {
          val sysOrPub = readNCName()
          space1("75", "space required after SYSTEM or PUBLIC")
          sysOrPub match {
            case "SYSTEM" =>
              Some(SYSTEM(readQuoted(false, "11")))
            case "PUBLIC" =>
              val pubid = readQuoted(true, "12")
              if (optionalPublicSystem) {
                val s = space()
                peekChar() match {
                  case Some(delimiter @ ('"' | '\'')) =>
                    if (s) {
                      Some(PUBLIC(pubid, Some(readQuoted(false, "12"))))
                    } else {
                      fail("75", "space required after PubidLiteral")
                    }
                  case _ =>
                    Some(PUBLIC(pubid, None))
                }
              } else {
                space1("75", "space required after PubidLiteral")
                Some(PUBLIC(pubid, Some(readQuoted(false, "12"))))
              }
            case _ =>
              fail("75", "SYSTEM or PUBLIC expected")
          }
        } else {
          fail("28", "space required befor ExternalId")
        }
      case _ =>
        None
    }
  }

  private def readQuoted(pub: Boolean, error: String): String = {
    space()
    val delimiter = assert(c => c == '"' || c == '\'', error, "single or double quote expected")
    val pred: Char => Boolean =
      if (pub)
        if (delimiter == '\'')
          c => !(c == 0x20 || c == 0xd || c == 0xa || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9') || "-'()+,./:=?;!*#@$_%".contains(c))
        else
          c => !(c == 0x20 || c == 0xd || c == 0xa || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9') || "-()+,./:=?;!*#@$_%".contains(c))
      else
        c => c == delimiter

    val s = untilChar(pred, new StringBuilder).toString
    nextChar()
    s
  }

}
