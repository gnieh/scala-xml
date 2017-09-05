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
    inputs: Queue[Source],
    var partial: Boolean) extends ParserBase(inputs, false) with Iterator[XmlEvent] with AutoCloseable {

  def this(input: Source, partial: Boolean) =
    this(Queue(input), partial)

  import XmlUtils._

  import XmlPullParser._

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

  // ==== low-level internals

  private var level = 0

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
      fail("5", f"character '$c' cannot start a NCName")
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

  /** Reads the nextChar markup token */
  private def readMarkupToken(): MarkupToken = {
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
            readCDATA(l, c)
          case _ =>
            DeclToken(readNCName(), l, c)
        }
      case _ =>
        StartToken(readQName(), l, c)
    }
  }

  /** We have read '<![' so far */
  private def readCDATA(l: Int, c: Int): CDataToken = {
    val n = read("CDATA[")
    if (n < 6)
      fail("19", "'CDATA[' expected")
    CDataToken(l, c)
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
          fail("16", "unexpected end of input")
      }
    }
    loop(new StringBuilder)
  }

  /** We read the beginning of internal DTD subset, read until final ']>' (included) */
  @tailrec
  private def rawInternalDTD(mayEnd: Boolean, sb: StringBuilder): String =
    (nextChar(): @switch) match {
      case ']' =>
        (nextChar(): @switch) match {
          case '>' => sb.append('>').toString
          case c   => rawInternalDTD(isXmlWhitespace(c), sb.append(']').append(c))
        }
      case '>' if mayEnd =>
        sb.append('>').toString
      case c =>
        rawInternalDTD(mayEnd && isXmlWhitespace(c), sb.append(c))
    }

  private def readExternalID(): Option[ExternalId] = {
    val s = space()
    peekChar() match {
      case Some(c) if isNCNameStart(c) =>
        if (s) {
          val sysOrPub = readNCName()
          assert(isXmlWhitespace(_), "75", "space required after SYSTEM or PUBLIC")
          space()
          sysOrPub match {
            case "SYSTEM" =>
              Some(SYSTEM(readQuoted(false, "11")))
            case "PUBLIC" =>
              val pubid = readQuoted(true, "12")
              assert(isXmlWhitespace(_), "75", "space required after PubidLiteral")
              space()
              Some(PUBLIC(pubid, readQuoted(false, "12")))
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
        case t                       => fail("22", f"unexpected token '$t'")
      }
      case Some(c) => fail("22", f"unexpected character '$c'")
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

  // ==== middle-level internals

  private def readAttributes(tname: QName, l: Int, c: Int): Either[Seq[Attr], ExpectAttributeValue] = {
    @tailrec
    def loop(attributes: VectorBuilder[Attr]): Either[Seq[Attr], ExpectAttributeValue] = {
      space()
      peekChar() match {
        case Some(c) if isNCNameStart(c) =>
          val name = readQName()
          space()
          accept('=', "25", "'=' character expected")
          space()
          if (partial && peekChar().isEmpty) {
            Right(ExpectAttributeValue(tname, attributes.result(), name)(l, c))
          } else {
            val delimiter = assert(c => c == '"' || c == '\'', "10", "single or double quote expected around attribute value")
            val value = readAttributeValue(Some(delimiter), new StringBuilder, new VectorBuilder, line, column)
            loop(attributes += Attr(name, value))
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
            builder += XmlEntitiyRef(s)(l, c)
            readAttributeValue(delim, new StringBuilder, builder, line, column)
        }
      case Some(c) =>
        fail("10", f"unexpected character '$c'")
    }
  }

  private def readNamedEntity(): String = {
    val name = readNCName()
    accept(';', "68", "named entity must end with a semicolon")
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
          accept('>', "44", "missing closing '>'")
          val builder = new VectorBuilder[Attr]

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
            fail("14", f"unexpected declaration '$n'")
          case CDataToken(l, c) if level > 0 =>
            val body = readCDATABody(new StringBuilder)
            XmlString(body, true)(l, c)
          case EndToken(name, l, c) =>
            EndTag(name)(l, c)
          case StartToken(name, l, c) =>
            completeStartTag(name, l, c)
          case PIToken(target, l, c) if !target.equalsIgnoreCase("xml") =>
            val body = readPIBody()
            position = 3
            XmlPI(target, body)(l, c)
          case t =>
            fail("43", f"unexpected token $t")
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
        if (isXmlWhitespace(c)) {
          space()
          readCharData()
        } else {
          fail("27", "only space character data is allowed")
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
              fail("14", "character data may not contain ']]>'")
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

  private var position = 0

  private def scanPrologToken0(): XmlEvent = {
    peekChar() match {
      case Some('<') =>
        readMarkupToken() match {
          case PIToken(name, l, c) if name == "xml" =>
            handleXmlDecl(l, c)
          case PIToken(name, l, c) if !name.equalsIgnoreCase("xml") =>
            val body = readPIBody()
            position = 0
            XmlPI(name, body)(l, c)
          case DeclToken(name, l, c) =>
            handleDecl(name, l, c)
          case StartToken(name, l, c) =>
            readElement(name, l, c)
          case CommentToken(_, _) =>
            scanPrologToken1()
          case t =>
            fail("22", f"unexpected markup $t")
        }
      case _ =>
        scanPrologToken1()
    }
  }

  private def scanPrologToken1(): XmlEvent = {
    scanMisc() match {
      case None =>
        fail("22", "unexpected end of input")
      case Some(PIToken(name, l, c)) if !name.equalsIgnoreCase("xml") =>
        val body = readPIBody()
        position = 1
        XmlPI(name, body)(l, c)
      case Some(DeclToken(name, l, c)) =>
        handleDecl(name, l, c)
      case Some(StartToken(name, l, c)) =>
        readElement(name, l, c)
      case Some(t) =>
        fail("22", f"unexpected markup $t")
    }
  }

  private def handleXmlDecl(l: Int, c: Int): XmlEvent = {
    assert(isXmlWhitespace(_), "24", "space is expected after xml")
    space()
    val n = read("version")
    if (n == 7) {
      space()
      accept('=', "24", "expected '=' after version")
      space()
      val delimiter = assert(c => c == '"' || c == '\'', "24", "simple or double quote expected")
      accept('1', "26", "expected major version 1")
      accept('.', "26", "expected dot")
      val version = untilChar(!_.isDigit, new StringBuilder("1.")).toString
      if (version.length == 2)
        fail("26", "expected non empty minor version")
      accept(delimiter, "24", "expected delimiter to close version attribute value")

      is11 = version == "1.1"

      val (hasSpace, encoding) = readEncoding(false)
      val standalone = readStandalone(hasSpace)
      space()
      (nextChar(): @switch) match {
        case '?' =>
          (nextChar(): @switch) match {
            case '>' => XmlDecl(version, encoding, standalone)(l, c)
            case _ =>
              fail("23", "expected end of PI")
          }
        case _ =>
          fail("23", "expected end of PI")
      }
    } else {
      fail("24", "expected 'version' attribute")
    }
  }

  @tailrec
  private def readEncoding(hasSpace: Boolean): (Boolean, Option[String]) =
    peekChar() match {
      case Some(c) if isXmlWhitespace(c) =>
        space()
        readEncoding(true)
      case Some('e') =>
        if (hasSpace) {
          val n = read("encoding")
          if (n != 8)
            fail("80", "expected 'encoding' attribute")
          space()
          accept('=', "80", "expected '='")
          space()
          val delimiter = assert(c => c == '"' || c == '\'', "80", "simple or double quote expected")
          val fst = assert(c => (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'), "81", "wrong encoding name character")
          val encoding = untilChar(c => !((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '.' || c == '_' || c == '-'), new StringBuilder().append(fst)).toString
          accept(delimiter, "80", "'encoding' attribute value must end with proper delimiter")
          (false, Some(encoding))
        } else {
          fail("80", "expected space before 'encoding' attrbute")
        }
      case _ =>
        (hasSpace, None)
    }

  @tailrec
  private def readStandalone(hasSpace: Boolean): Option[Boolean] =
    peekChar() match {
      case Some(c) if isXmlWhitespace(c) =>
        space()
        readStandalone(true)
      case Some('s') =>
        if (hasSpace) {
          val n = read("standalone")
          if (n != 10)
            fail("32", "expected 'standalone' attribute")
          space()
          accept('=', "32", "expected '='")
          space()
          val delimiter = assert(c => c == '"' || c == '\'', "32", "simple or double quote expected")
          val value = (nextChar(): @switch) match {
            case 'y' =>
              if (read("es") == 2)
                true
              else
                fail("32", "expected 'yes' or 'no'")
            case 'n' =>
              accept('o', "32", "expected 'yes' or 'no'")
              false
            case _ =>
              fail("32", "expected 'yes' or 'no'")
          }
          accept(delimiter, "32", "'standalone' attribute value must end with proper delimiter")
          Some(value)
        } else {
          fail("32", "expected space before 'standalone' attrbute")
        }
      case _ =>
        None
    }

  private def handleDecl(name: String, l: Int, c: Int): XmlEvent =
    name match {
      case "DOCTYPE" =>
        assert(isXmlWhitespace(_), "28", "space is expected after DOCTYPE")
        space()
        val docname = readNCName()
        val externalid = readExternalID()
        space()
        nextChar() match {
          case '>' =>
            // done
            XmlDoctype(name, docname, externalid, None)(l, c)
          case '[' =>
            val dtd = rawInternalDTD(false, new StringBuilder)
            XmlDoctype(name, docname, externalid, Some(dtd))(l, c)
          case c =>
            fail("28", "expected end of doctype or internal DTD")
        }
      case _ =>
        fail("22", "expected DOCTYPE declaration")
    }

  private def scanPrologToken2(): XmlEvent =
    scanMisc() match {
      case None =>
        fail("22", "unexpected end of input")
      case Some(PIToken(name, l, c)) if !name.equalsIgnoreCase("xml") =>
        val body = readPIBody()
        position = 2
        XmlPI(name, body)(l, c)
      case Some(StartToken(name, l, c)) =>
        readElement(name, l, c)
      case Some(t) =>
        fail("22", f"unexpected markup $t")
    }

  private def scanPostlog(): XmlEvent =
    scanMisc() match {
      case None => EndDocument()(line, column)
      case Some(PIToken(name, l, c)) if !name.equalsIgnoreCase("xml") =>
        val body = readPIBody()
        position = 4
        XmlPI(name, body)(l, c)
      case Some(t) =>
        fail("1", f"unexpected markup $t after root element")
    }

  private def scan(): XmlEvent = previousEvent match {
    case StartDocument =>
      scanPrologToken0()
    case XmlDecl(_, _, _) =>
      scanPrologToken1()
    case XmlDoctype(_, _, _, _) =>
      scanPrologToken2()
    case StartTag(name, _, true) =>
      level += 1
      EndTag(name)(previousEvent.line, previousEvent.column)
    case StartTag(_, _, _) =>
      level += 1
      readContent()
    case EndTag(_) =>
      level -= 1
      if (level > 0)
        readCharData()
      else
        scanPostlog()
    case XmlString(_, _) =>
      readCharData()
    case XmlCharRef(_) =>
      readCharData()
    case XmlEntitiyRef(_) =>
      readCharData()
    case XmlPI(_, _) =>
      if (position == 0)
        scanPrologToken0()
      else if (position == 1)
        scanPrologToken1()
      else if (position == 2)
        scanPrologToken2()
      else if (position == 3)
        readCharData()
      else
        scanPostlog()
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
