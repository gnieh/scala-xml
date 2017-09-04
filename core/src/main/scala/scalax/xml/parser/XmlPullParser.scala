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

import scala.util.Try

import scala.annotation.{
  tailrec,
  switch
}

import scala.collection.immutable.{
  VectorBuilder,
  Queue
}

/** An XML pull parser that accepts some form of partial inputs.
 *  The parser does not perform any qualified name resolution and
 *  does not track qualified name resolution nor namespaces.
 *  Namespace declarations are returned as attributes of the declaring element.
 *  Entities and character references are not processed either.
 *
 *  This parser implements the XML Namespaces recommendation (see https://www.w3.org/TR/xml-names/).
 */
class XmlPullParser private (
    private var inputs: Queue[Source],
    var partial: Boolean) extends Iterator[XmlEvent] {

  def this(input: Source, partial: Boolean) =
    this(Queue(input), partial)

  import XmlPullParser._

  private var is11 = false

  private var nextEvent: XmlEvent = StartDocument
  private var previousEvent: XmlEvent = null

  def hasNext(): Boolean = nextEvent match {
    case null =>
      previousEvent match {
        case EndDocument() =>
          false
        case _ =>
          nextEvent = scan()
          nextEvent != null
      }
    case _ =>
      true
  }

  def next(): XmlEvent = nextEvent match {
    case null =>
      val evt = scan()
      previousEvent = evt
      evt
    case _ =>
      previousEvent = nextEvent
      nextEvent = null
      previousEvent
  }

  /** Feeds this parser with more content from the given source. */
  def feed(src: Source): Unit =
    inputs = inputs.enqueue(src)

  // ==== low-level internals

  private def currentInput = inputs.headOption

  private def line = currentInput.fold(0)(_.RelaxedPositioner.cline)

  private def column = currentInput.fold(0)(_.RelaxedPositioner.ccol)

  private var level = 0

  private def fail(msg: String): Nothing = {
    Try(inputs.foreach(_.close()))
    throw new ParserException(line, column, msg)
  }

  private var buffer = Queue.empty[Char]

  @tailrec
  private def readInput(): Unit =
    inputs.dequeueOption match {
      case Some((input, rest)) =>
        if (input.hasNext) {
          val c = input.next()
          if (isValid(c))
            buffer = buffer.enqueue(c)
          else
            fail("XML [2]: forbidden character")
        } else {
          input.close()
          inputs = rest
          readInput()
        }
      case None =>
        buffer = Queue.empty[Char]
    }

  private def peekChar(): Option[Char] =
    buffer.headOption.orElse {
      readInput()
      buffer.headOption
    }

  private def nextCharOpt(): Option[Char] = {
    val c = peekChar()
    if (buffer.nonEmpty)
      buffer = buffer.tail
    c
  }

  /** Consumes the next character in source and returns it. */
  private def nextChar(): Char =
    nextCharOpt().getOrElse(fail("Unexpected end of input"))

  private def accept(c: Char, msg: String): Unit = peekChar() match {
    case Some(n) if n == c =>
      nextChar()
    case _ =>
      fail(msg)
  }

  private def assert(p: Char => Boolean, msg: String): Char = peekChar() match {
    case Some(c) if p(c) =>
      nextChar()
    case _ =>
      fail(msg)
  }

  private def untilChar(p: Char => Boolean, sb: StringBuilder): StringBuilder = {
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

  private def read(s: String): Int = {
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

  private def isValid(c: Int): Boolean =
    if (is11)
      // [#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
      (0x1 <= c && c <= 0xd7ff) || (0xe000 <= c && c <= 0xfffd) || (0x10000 <= c && c <= 0x10ffff)
    else
      // #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
      c == 0x9 || c == 0xa || c == 0xd || (0x20 <= c && c <= 0xd7ff) || (0xe000 <= c && c <= 0xfffd) || (0x10000 <= c && c <= 0x10ffff)

  private def isSpace(c: Char): Boolean =
    c == ' ' || c == '\t' || c == '\r' || c == '\n'

  private def isNCNameStart(c: Char): Boolean = {
    import java.lang.Character._

    getType(c).toByte match {
      case LOWERCASE_LETTER |
        UPPERCASE_LETTER | OTHER_LETTER |
        TITLECASE_LETTER | LETTER_NUMBER => true
      case _ => "_".contains(c)
    }
  }

  private def isNCNameChar(c: Char): Boolean = {
    import java.lang.Character._
    // The constants represent groups Mc, Me, Mn, Lm, and Nd.

    isNCNameStart(c) || (getType(c).toByte match {
      case COMBINING_SPACING_MARK |
        ENCLOSING_MARK | NON_SPACING_MARK |
        MODIFIER_LETTER | DECIMAL_DIGIT_NUMBER => true
      case _ => ".-·".contains(c)
    })
  }

  private def readNCName(): String = {
    val c = nextChar()
    if (isNCNameStart(c)) {
      val sb = new StringBuilder
      untilChar(c => !isNCNameChar(c), sb.append(c))
      sb.toString
    } else {
      fail(f"XML [5]: character '$c' cannot start a NCName")
    }
  }

  private def readQName(): QName = {
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

  private def space(): Unit = {
    @tailrec
    def loop(): Unit =
      peekChar() match {
        case Some(c) if isSpace(c) =>
          nextChar()
          loop()
        case _ =>
        // done
      }
    loop()
  }

  /** Reads the nextChar markup token */
  private def readMarkupToken(): MarkupToken = {
    accept('<', "XML [43]: expected token start")
    val l = line
    val c = column
    peekChar() match {
      case Some('/') =>
        nextChar()
        val qname = readQName()
        space()
        accept('>', "XML [42]: missing '>' at the end of closing tag")
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
            readCDATA(l, c)
          case _ =>
            DeclToken(readNCName(), l, c)
        }
      case _ =>
        StartToken(readQName(), l, c)
    }
  }

  /** We have read '<!-' so far */
  private def skipComment(l: Int, c: Int): CommentToken = {
    accept('-', "XML [15]: second dash missing to open comment")
    def loop(): Unit = (nextChar(): @switch) match {
      case '-' =>
        (nextChar(): @switch) match {
          case '-' =>
            accept('>', "XML [15]: '--' is not inside comments")
          case _ =>
            loop()
        }
      case _ =>
        loop()
    }
    loop()
    CommentToken(l, c)
  }

  /** We have read '<![' so far */
  private def readCDATA(l: Int, c: Int): CDataToken = {
    val n = read("CDATA[")
    if (n < 6)
      fail("XML [19]: 'CDATA[' expected")
    CDataToken(l, c)
  }

  /** inside a PI read until final '?>' */
  @tailrec
  private def skipPI(): Unit =
    (nextChar(): @switch) match {
      case '?' =>
        (peekChar(): @switch) match {
          case Some('>') => nextChar()
          case _         => skipPI()
        }
      case _ => skipPI()
    }

  /** We have just read the PI target */
  private def readPIBody(): String = {
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
          fail("XML [16]: Unexpected end of input")
      }
    }
    loop(new StringBuilder)
  }

  /** We read the beginning of internal DTD subset, read until final ']>' */
  @tailrec
  private def skipInternalDTD(): Unit =
    (nextChar(): @switch) match {
      case ']' =>
        (nextChar(): @switch) match {
          case '>' => // done
          case _   => skipInternalDTD()
        }
      case _ => skipInternalDTD()
    }

  private def readExternalID(): String = {
    val sysOrPub = readNCName()
    assert(isSpace(_), "XML [75]: space required after SYSTEM or PUBLIC")
    sysOrPub match {
      case "SYSTEM" =>
        readQuoted(false)
      case "PUBLIC" =>
        readQuoted(true)
        assert(isSpace(_), "XML [12]: space required after PubidLiteral")
        readQuoted(false)
      case _ =>
        fail("XML [75]: SYSTEM or PUBLIC expected")
    }
  }

  private def readQuoted(pub: Boolean): String = {
    space()
    val delimiter = assert(c => c == '"' || c == '\'', "XML [11], XML [12]: single or double quote expected")
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

  @tailrec
  private def scanMisc(): Option[MarkupToken] = {
    space()
    peekChar() match {
      case None => None
      case Some('<') => readMarkupToken() match {
        case CommentToken(_, _)      => scanMisc()
        case t @ PIToken(_, _, _)    => Some(t)
        case t @ DeclToken(_, _, _)  => Some(t)
        case t @ StartToken(_, _, _) => Some(t)
        case t                       => fail(f"XML [22]: unexpected token '$t'")
      }
      case Some(c) => fail(f"XML [22]: unexpected character '$c'")
    }
  }

  /** We read '&#' so far */
  def readCharRef(): Int = {
    def postlude(n: Int) =
      (nextChar(): @switch) match {
        case ';' =>
          if (isValid(n))
            n
          else
            fail("XML [2]: invalid character")
        case _ =>
          fail("XML [66]: character reference must end with a semicolon")
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
      case _        => fail("XML [66]: bad first character reference digit")
    }
  }

  // ==== middle-level internals

  private def readAttributes(tname: QName, l: Int, c: Int): Either[Attributes, ExpectAttributeValue] = {
    @tailrec
    def loop(attributes: VectorBuilder[Attribute]): Either[Attributes, ExpectAttributeValue] = {
      space()
      peekChar() match {
        case Some(c) if isNCNameStart(c) =>
          val name = readQName()
          space()
          accept('=', "XML [25]: '=' character expected")
          space()
          if (partial && peekChar().isEmpty) {
            Right(ExpectAttributeValue(tname, attributes.result(), name)(l, c))
          } else {
            val delimiter = assert(c => c == '"' || c == '\'', "XML [10]: single or double quote expected around attribute value")
            val value = readAttributeValue(Some(delimiter), new StringBuilder, new VectorBuilder, line, column)
            loop(attributes += Attribute(name, value))
          }
        case _ =>
          Left(attributes.result())
      }
    }
    loop(new VectorBuilder)
  }

  @tailrec
  private def readAttributeValue(delim: Option[Char], current: StringBuilder, builder: VectorBuilder[XmlTexty], l: Int, c: Int): Seq[XmlTexty] = {
    val delimiters = delim.fold(valueDelimiters)(valueDelimiters + _)
    untilChar(delimiters.contains(_), current)
    nextCharOpt() match {
      case `delim` =>
        if (!current.isEmpty)
          builder += XmlString(current.toString, false)(l, c)
        builder.result()
      case None => fail("XML [10]: unexpected end of input")
      case Some('\r') =>
        peekChar() match {
          case Some('\n') =>
            readAttributeValue(delim, current.append(nextChar()), builder, l, c)
          case _ =>
            readAttributeValue(delim, current.append(' '), builder, l, c)
        }
      case Some(c) if isSpace(c) =>
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
            builder += XmlEntitiyRef(s)(l, c)
            readAttributeValue(delim, new StringBuilder, builder, line, column)
        }
      case Some(c) =>
        fail(f"XML [10]: Unexpected character '$c'")
    }
  }

  private def readNamedEntity(): String = {
    val name = readNCName()
    accept(';', "XML [68]: named entity must end with a semicolon")
    name
  }

  private def completeStartTag(name: QName, l: Int, c: Int): XmlEvent = {
    readAttributes(name, l, c) match {
      case Left(attributes) =>
        space()
        if (partial && peekChar().isEmpty) {
          // end of current inputs but partial parse is enabled
          // emit the appropriate event to feed with custom attributes
          // before continuing.
          ExpectAttributes(name, attributes)(l, c)
        } else {
          val isEmpty = peekChar() match {
            case Some('/') =>
              nextChar()
              true
            case _ =>
              false
          }
          accept('>', "XML [44]: missing closing '>'")
          val builder = new VectorBuilder[Attribute]

          StartTag(name, attributes, isEmpty)(l, c)
        }
      case Right(evt) =>
        evt
    }
  }

  /** We read '<[CDATA[' so far */
  @tailrec
  private def readCDATABody(sb: StringBuilder): String = {

    untilChar(c => c == '\n' || c == '\r' || c == ']' || c == '&', sb)
    (nextChar(): @switch) match {
      case '\n' =>
        sb.append('\n')
        readCDATABody(sb)
      case ']' =>
        peekChar() match {
          case Some(']') =>
            nextChar()
            if (checkCDATAEnd(sb))
              sb.toString
            else
              readCDATABody(sb)
          case _ =>
            sb.append(']')
            readCDATABody(sb)
        }
      case '&' =>
        val n = read("gt;")
        if (n == 3) {
          sb.append('>')
        } else {
          sb.append('&')
          for (i <- 0 until n)
            sb.append("gt;".charAt(i))
        }
        readCDATABody(sb)
      case _ =>
        // must be '\r'
        peekChar() match {
          case Some('\n') =>
            sb.append(nextChar())
          case _ =>
            sb.append('\n')
        }
        readCDATABody(sb)
    }
  }

  def checkCDATAEnd(sb: StringBuilder): Boolean =
    peekChar() match {
      case Some('>') =>
        // done
        nextChar()
        true
      case Some(']') =>
        nextChar()
        sb.append(']')
        checkCDATAEnd(sb)
      case _ =>
        sb.append("]]")
        false
    }

  @tailrec
  private def readCharData(): XmlEvent =
    peekChar() match {
      case Some('<') =>
        readMarkupToken() match {
          case CommentToken(l, c) =>
            readCharData()
          case DeclToken(n, l, c) =>
            fail(f"Unexpected declaration '$n'")
          case CDataToken(l, c) if level > 0 =>
            val body = readCDATABody(new StringBuilder)
            XmlString(body, true)(l, c)
          case EndToken(name, l, c) =>
            EndTag(name)(l, c)
          case StartToken(name, l, c) =>
            completeStartTag(name, l, c)
          case PIToken(target, l, c) if !target.equalsIgnoreCase("xml") =>
            skipPI()
            readCharData()
          case t =>
            fail(f"XML [43]: unexpected token $t")
        }
      case Some('&') if level > 0 =>
        val l = line
        val c = column
        nextChar()
        peekChar() match {
          case Some('#') =>
            nextChar()
            val n = readCharRef()
            XmlCharRef(n)(l, c)
          case _ =>
            val v = readNamedEntity()
            XmlEntitiyRef(v)(l, c)
        }
      case Some(c) if level == 0 =>
        // at the root level, only space characters are allowed
        if (isSpace(c)) {
          space()
          readCharData()
        } else {
          fail("XML [27]: only space character data is allowed")
        }
      case Some(_)         => slowPath(new StringBuilder, line, column)
      case None if partial => ExpectNodes()(line, column)
      case None            => EndDocument()(line, column)
    }

  @tailrec
  private def slowPath(sb: StringBuilder, l: Int, c: Int): XmlEvent = {
    untilChar(c => c == '<' || c == '&' || c == '\r' || c == ']', sb)
    peekChar() match {
      case Some('<') => XmlString(sb.toString, false)(l, c)
      case None      => XmlString(sb.toString, false)(l, c)
      case Some('&') => XmlString(sb.toString, false)(l, c)
      case Some(']') =>
        nextChar()
        peekChar() match {
          case Some(']') =>
            nextChar()
            if (checkCDATAEnd(sb))
              fail("XML [14]: character data may not contain ']]>'")
            else
              slowPath(sb, l, c)
          case _ =>
            slowPath(sb.append(']'), l, c)
        }
      case _ =>
        nextChar()
        peekChar() match {
          case Some('\n') =>
            sb.append(nextChar())
            slowPath(sb, l, c)
          case _ =>
            sb.append('\n')
            slowPath(sb, l, c)
        }
    }
  }

  private def readElement(start: QName, l: Int, c: Int): XmlEvent =
    completeStartTag(start, l, c)

  private def readContent(): XmlEvent =
    if (partial && peekChar().isEmpty)
      ExpectNodes()(line, column)
    else
      readCharData()

  // ==== high-level internals

  private def scanPrologToken0(): XmlEvent = {
    peekChar() match {
      case Some('<') =>
        readMarkupToken() match {
          case PIToken(name, l, c) if name == "xml" =>
            handleXmlDecl(l, c)
          case PIToken(name, l, c) if !name.equalsIgnoreCase("xml") =>
            skipPI()
            scanPrologToken1()
          case DeclToken(name, l, c) =>
            handleDecl(name, l, c)
          case StartToken(name, l, c) =>
            readElement(name, l, c)
          case CommentToken(_, _) =>
            scanPrologToken1()
          case t =>
            fail(f"XML [22]: unexpected markup $t")
        }
      case _ =>
        scanPrologToken1()
    }
  }

  @tailrec
  private def scanPrologToken1(): XmlEvent = {
    scanMisc() match {
      case None =>
        fail("XML [22]: unexpected end of input")
      case Some(PIToken(name, l, c)) if !name.equalsIgnoreCase("xml") =>
        skipPI()
        scanPrologToken1()
      case Some(DeclToken(name, l, c)) =>
        handleDecl(name, l, c)
      case Some(StartToken(name, l, c)) =>
        readElement(name, l, c)
      case Some(t) =>
        fail(f"XML [22]: unexpected markup $t")
    }
  }

  private def handleXmlDecl(l: Int, c: Int): XmlEvent = {
    assert(isSpace(_), "XML [24]: space is expected after xml")
    space()
    val n = read("version")
    if (n == 7) {
      space()
      accept('=', "XML [24]: expected '=' after version")
      space()
      val delimiter = assert(c => c == '"' || c == '\'', "XML [24]: simple or double quote expected")
      accept('1', "XML [26]: expected major version 1")
      accept('.', "XML [26]: expected dot")
      val version = untilChar(!_.isDigit, new StringBuilder("1.")).toString
      if (version.length == 2)
        fail("XML [26]: expected non empty minor version")
      accept(delimiter, "XML [24]: expected delimiter to close version attribute value")

      is11 = version == "1.1"

      val (hasSpace, encoding) = readEncoding(false)
      val standalone = readStandalone(hasSpace)
      space()
      (nextChar(): @switch) match {
        case '?' =>
          (nextChar(): @switch) match {
            case '>' => XmlDecl(version, encoding, standalone)(l, c)
            case _ =>
              fail("XML [23]: expected end of PI")
          }
        case _ =>
          fail("XML [23]: expected end of PI")
      }
    } else {
      fail("XML [24]: expected 'version' attribute")
    }
  }

  @tailrec
  private def readEncoding(hasSpace: Boolean): (Boolean, Option[String]) =
    peekChar() match {
      case Some(c) if isSpace(c) =>
        space()
        readEncoding(true)
      case Some('e') =>
        if (hasSpace) {
          val n = read("encoding")
          if (n != 8)
            fail("XML [80]: expected 'encoding' attribute")
          space()
          accept('=', "XML [80]: expected '='")
          space()
          val delimiter = assert(c => c == '"' || c == '\'', "XML [80]: simple or double quote expected")
          val fst = assert(c => (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'), "XML [81]: wrong encoding name character")
          val encoding = untilChar(c => !((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '.' || c == '_' || c == '-'), new StringBuilder().append(fst)).toString
          accept(delimiter, "XML [80]: 'encoding' attribute value must end with proper delimiter")
          (false, Some(encoding))
        } else {
          fail("XML [80]: expected space before 'encoding' attrbute")
        }
      case _ =>
        (hasSpace, None)
    }

  @tailrec
  private def readStandalone(hasSpace: Boolean): Option[Boolean] =
    peekChar() match {
      case Some(c) if isSpace(c) =>
        space()
        readStandalone(true)
      case Some('s') =>
        if (hasSpace) {
          val n = read("standalone")
          if (n != 10)
            fail("XML [32]: expected 'standalone' attribute")
          space()
          accept('=', "XML [32]: expected '='")
          space()
          val delimiter = assert(c => c == '"' || c == '\'', "XML [32]: simple or double quote expected")
          val value = (nextChar(): @switch) match {
            case 'y' =>
              if (read("es") == 2)
                true
              else
                fail("XML [32]: expected 'yes' or 'no'")
            case 'n' =>
              accept('o', "XML [32]: expected 'yes' or 'no'")
              false
            case _ =>
              fail("XML [32]: expected 'yes' or 'no'")
          }
          accept(delimiter, "XML [32]: 'standalone' attribute value must end with proper delimiter")
          Some(value)
        } else {
          fail("XML [32]: expected space before 'standalone' attrbute")
        }
      case _ =>
        None
    }

  private def handleDecl(name: String, l: Int, c: Int): XmlEvent =
    name match {
      case "DOCTYPE" =>
        assert(isSpace(_), "XML [28]: space is expected after DOCTYPE")
        space()
        val docname = readNCName()
        space()
        val systemid = peekChar() match {
          case Some(c) if isNCNameStart(c) => Some(readExternalID())
          case _                           => None
        }
        space()
        nextChar() match {
          case '>' =>
            // done
            XmlDoctype(name, docname, systemid)(l, c)
          case '[' =>
            skipInternalDTD()
            XmlDoctype(name, docname, systemid)(l, c)
          case c =>
            fail(f"XML [28]: end of doctype or internal DTD expected but got $c")
        }
      case _ =>
        fail("XML [22]: expected DOCTYPE declaration")
    }

  @tailrec
  private def scanPrologToken2(): XmlEvent =
    scanMisc() match {
      case None =>
        fail("XML [22]: unexpected end of input")
      case Some(PIToken(name, l, c)) if !name.equalsIgnoreCase("xml") =>
        skipPI()
        scanPrologToken2()
      case Some(StartToken(name, l, c)) =>
        readElement(name, l, c)
      case Some(t) =>
        fail(f"XML [22]: unexpected markup $t")
    }

  private def scan(): XmlEvent = previousEvent match {
    case StartDocument =>
      scanPrologToken0()
    case XmlDecl(_, _, _) =>
      scanPrologToken1()
    case XmlDoctype(_, _, _) =>
      scanPrologToken2()
    case StartTag(name, _, true) =>
      level += 1
      EndTag(name)(previousEvent.line, previousEvent.column)
    case StartTag(_, _, _) =>
      level += 1
      readContent()
    case EndTag(_) =>
      level -= 1
      readCharData()
    case XmlString(_, _) =>
      readCharData()
    case XmlCharRef(_) =>
      readCharData()
    case XmlEntitiyRef(_) =>
      readCharData()
    case EndDocument() =>
      throw new NoSuchElementException
    case ExpectAttributes(name, _) =>
      completeStartTag(name, previousEvent.line, previousEvent.column)
    case ExpectAttributeValue(name, _, _) =>
      completeStartTag(name, previousEvent.line, previousEvent.column)
    case ExpectNodes() =>
      readContent()
  }

}

private object XmlPullParser {

  val valueDelimiters = " \t\r\n<&"

}
