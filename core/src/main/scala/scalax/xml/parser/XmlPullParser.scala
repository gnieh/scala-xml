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

import java.net.URI

/** An XML pull parser that accepts some form of partial inputs.
 *  The parser performs qualified name resolution base on current `namespaces`.
 */
class XmlPullParser private (
    private var inputs: Queue[Source],
    predefEntities: Map[String, String],
    private var namespaces: NameSpaces,
    var partial: Boolean) extends Iterator[XmlEvent] {

  def this(input: Source, predefEntities: Map[String, String], namespaces: NameSpaces, partial: Boolean) =
    this(Queue(input), predefEntities, namespaces, partial)

  import XmlPullParser._

  private var entities = Map.empty[String, NamedEntityBody]

  private var nextEvent: XmlEvent = StartDocument
  private var previousEvent: XmlEvent = null

  def hasNext(): Boolean = nextEvent match {
    case null =>
      previousEvent match {
        case EndDocument =>
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

  private var sawCR = false
  private var line = 1
  private var column = 0

  private def fail(msg: String): Nothing = {
    Try(inputs.foreach(_.close()))
    throw new ParserException(line, column, msg)
  }

  private var buffer = Option.empty[Char]

  @tailrec
  private def readInput(): Unit =
    inputs.dequeueOption match {
      case Some((input, rest)) =>
        if (input.hasNext) {
          val c = input.next()
          c match {
            case '\n' =>
              line += 1
              column = 0
              sawCR = false
            case '\r' =>
              sawCR = true
            case _ if sawCR =>
              line += 1
              column = 0
              sawCR = false
            case _ =>
              sawCR = false
              column += 1
          }
          buffer = Some(c)
        } else {
          input.close()
          inputs = rest
          readInput()
        }
      case None =>
        buffer = None
    }

  private def peekChar(): Option[Char] =
    buffer.orElse {
      readInput()
      buffer
    }

  private def nextCharOpt(): Option[Char] = {
    val c = peekChar()
    buffer = None
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

  private def isNCNameStart(c: Char): Boolean = {
    import java.lang.Character._

    getType(c).toByte match {
      case LOWERCASE_LETTER |
        UPPERCASE_LETTER | OTHER_LETTER |
        TITLECASE_LETTER | LETTER_NUMBER => true
      case _ => ":_".contains(c)
    }
  }

  private def isNCNameChar(c: Char): Boolean = {
    import java.lang.Character._
    // The constants represent groups Mc, Me, Mn, Lm, and Nd.

    isNCNameStart(c) || (getType(c).toByte match {
      case COMBINING_SPACING_MARK |
        ENCLOSING_MARK | NON_SPACING_MARK |
        MODIFIER_LETTER | DECIMAL_DIGIT_NUMBER => true
      case _ => ".-:·".contains(c)
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

  private def resolveQName(name: QName, applyDefault: Boolean): QName =
    name match {
      case QName(None, _, _) if !applyDefault =>
        name
      case QName(None, n, _) =>
        namespaces.get("") match {
          case Some(uri) => QName(None, n, Some(uri))
          case None      => name
        }
      case QName(Some(ns), n, _) =>
        namespaces.get(ns) match {
          case Some(uri)                          => QName(Some(ns), n, Some(uri))
          case None if ns.equalsIgnoreCase("xml") => QName(Some(ns), n, Some(xmlNSURI))
          case None                               => fail(f"[nsc-NSDeclared]: undeclared namespace $ns")
        }
    }

  private def space(): Unit = {
    @tailrec
    def loop(): Unit =
      peekChar() match {
        case Some(c) if c.isWhitespace =>
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
    peekChar() match {
      case Some('/') =>
        nextChar()
        val qname = readQName()
        space()
        accept('>', "XML [42]: missing '>' at the end of closing tag")
        EndToken(qname)
      case Some('?') =>
        nextChar()
        PIToken(readNCName())
      case Some('!') =>
        nextChar()
        peekChar() match {
          case Some('-') =>
            nextChar()
            skipComment()
          case Some('[') =>
            nextChar()
            readCDATA()
          case _ =>
            DeclToken(readNCName())
        }
      case _ =>
        StartToken(readQName())
    }
  }

  /** We have read '<!-' so far */
  private def skipComment(): CommentToken.type = {
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
    CommentToken
  }

  /** We have read '<![' so far */
  private def readCDATA(): CDataToken.type = {
    val n = read("CDATA[")
    if (n < 6)
      fail("XML [19]: 'CDATA[' expected")
    CDataToken
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
    assert(_.isWhitespace, "XML [75]: space required after SYSTEM or PUBLIC")
    sysOrPub match {
      case "SYSTEM" =>
        readQuoted()
      case "PUBLIC" =>
        readQuoted()
        assert(_.isWhitespace, "XML [12]: space required after PubidLiteral")
        readQuoted()
      case _ =>
        fail("XML [75]: SYSTEM or PUBLIC expected")
    }
  }

  private def readQuoted(): String = {
    space()
    val delimiter = assert(c => c == '"' || c == '\'', "XML [11], XML [12]: single or double quote expected")
    val s = untilChar(c => c == delimiter, new StringBuilder).toString
    nextChar()
    s
  }

  @tailrec
  private def scanMisc(): Option[MarkupToken] = {
    space()
    peekChar() match {
      case None => None
      case Some('<') => readMarkupToken() match {
        case CommentToken      => scanMisc()
        case t @ PIToken(_)    => Some(t)
        case t @ DeclToken(_)  => Some(t)
        case t @ StartToken(_) => Some(t)
        case t                 => fail(f"XML [22]: unexpected token '$t'")
      }
      case Some(c) => fail(f"XML [22]: unexpected character '$c'")
    }
  }

  /** We read '&#' so far */
  def readCharRef(): Char = {
    def postlude(n: Int) =
      (nextChar(): @switch) match {
        case ';' => n.toChar
        case _   => fail("XML [66]: character reference must end with a semicolon")
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
              Some(c - 'a')
            else if (base == 16 && 'A' <= c && c <= 'F')
              Some(c - 'A')
            else
              None
          case None =>
            None
        }
    }

    @tailrec
    def rest(acc: Int): Char =
      peekChar() match {
        case Digit(d) =>
          nextChar()
          rest(acc * base + d)
        case _ =>
          acc.toChar
      }

    nextCharOpt() match {
      case Digit(d) => rest(d)
      case _        => fail("XML [66]: bad first character reference digit")
    }
  }

  // ==== middle-level internals

  private def parsedEntitiy(name: String): String =
    entities.get(name) match {
      case Some(SourceNamedEntitiy(src)) =>
        entities = entities.updated(name, NEBlackHole)
        // TODO read content
        name
      case Some(NEBlackHole) =>
        fail(f"[norecursion]: entity $name is recursive")
      case None =>
        predefEntities.get(name) match {
          case Some(s) =>
            s
          case None =>
            fail(f"[wf-entdeclared]: Entitiy $name is not declared")
        }
    }

  private def readAttributes(tname: QName): Either[Attributes, ExpectAttributeValue] = {
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
            Right(ExpectAttributeValue(tname, attributes.result(), name))
          } else {
            val delimiter = assert(c => c == '"' || c == '\'', "XML [10]: single or double quote expected around attribute value")
            val value = readAttributeValue(Some(delimiter), new StringBuilder)
            loop(attributes += Attribute(name, value))
          }
        case _ =>
          Left(attributes.result())
      }
    }
    loop(new VectorBuilder)
  }

  @tailrec
  private def readAttributeValue(delim: Option[Char], sb: StringBuilder): String = {
    val delimiters = delim.fold(valueDelimiters)(valueDelimiters + _)
    untilChar(delimiters.contains(_), sb)
    nextCharOpt() match {
      case `delim` => sb.toString
      case None    => fail("XML [10]: unexpected end of input")
      case Some('\r') =>
        peekChar() match {
          case Some('\n') =>
            readAttributeValue(delim, sb.append(nextChar()))
          case _ =>
            readAttributeValue(delim, sb.append(' '))
        }
      case Some(c) if c.isWhitespace =>
        readAttributeValue(delim, sb.append(' '))
      case Some('&') =>
        peekChar() match {
          case Some('#') =>
            nextChar()
            val c = readCharRef()
            readAttributeValue(delim, sb.append(c))
          case _ =>
            val s = readNamedEntity()
            readAttributeValue(delim, sb.append(s))
        }
      case Some(c) =>
        fail(f"XML [10]: Unexpected character '$c'")
    }
  }

  private def readNamedEntity(): String = {
    val name = readNCName()
    accept(';', "XML [68]: named entity must end with a semicolon")
    parsedEntitiy(name)
  }

  private def completeStartTag(name: QName): XmlEvent = {
    readAttributes(name) match {
      case Left(attributes) =>
        space()
        if (partial && peekChar().isEmpty) {
          // end of current inputs but partial parse is enabled
          // emit the appropriate event to feed with custom attributes
          // before continuing.
          ExpectAttributes(name, attributes)
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
          @tailrec
          def adjustNS(attributes: Attributes): Attributes =
            attributes match {
              case Seq() => builder.result()
              case Seq(att, rest @ _*) =>
                att match {
                  case Attribute(QName(None, "xmlns", _), "") =>
                    // undeclare default namespace
                    namespaces -= ""
                  case Attribute(QName(None, "xmlns", _), v) =>
                    // update default namespace
                    namespaces += ("" -> new URI(v))
                  case Attribute(QName(Some("xmlns"), p, _), "") =>
                    // undeclare namespace
                    namespaces -= p
                  case Attribute(QName(Some("xmlns"), p, _), v) =>
                    // update namespace
                    namespaces += (p -> new URI(v))
                  case Attribute(name, value) =>
                    builder += Attribute(resolveQName(name, false), value)
                }
                adjustNS(rest)
            }

          val proper = adjustNS(attributes)

          val resolved = resolveQName(name, true)

          StartTag(resolved, proper, isEmpty)
        }
      case Right(evt) =>
        evt
    }
  }

  /** We read '<[CDATA[' so far */
  @tailrec
  private def readCDATABody(sb: StringBuilder): String = {

    def checkEnd(): Boolean =
      peekChar() match {
        case Some('>') =>
          // done
          nextChar()
          true
        case Some(']') =>
          nextChar()
          sb.append(']')
          checkEnd()
        case _ =>
          sb.append("]]")
          false
      }

    untilChar(c => c == '\n' || c == '\r' || c == ']' || c == '&', sb)
    (nextChar(): @switch) match {
      case '\n' =>
        sb.append('\n')
        readCDATABody(sb)
      case ']' =>
        peekChar() match {
          case Some(']') =>
            nextChar()
            if (checkEnd())
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

  @tailrec
  private def readCharData(): XmlEvent =
    peekChar() match {
      case Some('<') =>
        readMarkupToken() match {
          case CommentToken =>
            readCharData()
          case DeclToken(n) =>
            fail(f"Unexpected declaration '$n'")
          case CDataToken =>
            val body = readCDATABody(new StringBuilder)
            XmlString(body, true)
          case EndToken(name) =>
            EndTag(name)
          case StartToken(name) =>
            completeStartTag(name)
          case PIToken(target) =>
            skipPI()
            readCharData()
          case t =>
            fail(f"XML [43]: unexpected token $t")
        }
      case Some(_)         => slowPath(new StringBuilder)
      case None if partial => ExpectNodes
      case None            => EndDocument
    }

  @tailrec
  private def slowPath(sb: StringBuilder): XmlEvent = {
    untilChar(c => c == '<' || c == '&' || c == '\r', sb)
    peekChar() match {
      case Some('<') => XmlString(sb.toString, false)
      case None      => XmlString(sb.toString, false)
      case Some('&') =>
        nextChar()
        peekChar() match {
          case Some('#') =>
            nextChar()
            sb.append(readCharRef())
            slowPath(sb)
          case _ =>
            val v = readNamedEntity()
            sb.append(v)
            slowPath(sb)
        }
      case _ =>
        nextChar()
        peekChar() match {
          case Some('\n') =>
            sb.append(nextChar())
            slowPath(sb)
          case _ =>
            sb.append('\n')
            slowPath(sb)
        }
    }
  }

  private def readElement(start: QName): XmlEvent =
    completeStartTag(start)

  private def readContent(): XmlEvent =
    if (partial && peekChar().isEmpty)
      ExpectNodes
    else
      readCharData()

  // ==== high-level internals

  private def scanPrologToken0(): XmlEvent = {
    scanMisc() match {
      case None =>
        fail("XML [22]: unexpected end of input")
      case Some(PIToken(name)) if name.equalsIgnoreCase("xml") =>
        handleXmlDexl()
      case Some(PIToken(name)) =>
        skipPI()
        scanPrologToken1()
      case Some(DeclToken(name)) =>
        handleDecl(name)
      case Some(StartToken(name)) =>
        readElement(name)
      case Some(t) =>
        fail(f"XML [22]: unexpected markup $t")
    }
  }

  @tailrec
  private def scanPrologToken1(): XmlEvent = {
    scanMisc() match {
      case None =>
        fail("XML [22]: unexpected end of input")
      case Some(PIToken(name)) =>
        skipPI()
        scanPrologToken1()
      case Some(DeclToken(name)) =>
        handleDecl(name)
      case Some(StartToken(name)) =>
        readElement(name)
      case Some(t) =>
        fail(f"XML [22]: unexpected markup $t")
    }
  }

  private def handleXmlDexl(): XmlEvent = {
    assert(_.isWhitespace, "XML [24]: space is expected after xml")
    space()
    val n = read("version")
    if (n == 7) {
      space()
      accept('=', "XML [24]: expected '=' after version")
      space()
      val delimiter = assert(c => c == '"' || c == '\'', "XML [24]: simple or double quote expected")
      accept('1', "XML [26]: expected major version 1")
      accept('.', "XML [26]: expected dot")
      val minor = untilChar(!_.isDigit, new StringBuilder("1.")).toString
      if (minor.length == 2)
        fail("XML [26]: expected non empty minor version")
      accept(delimiter, "XML [24]: expected delimiter to close version attribute value")

      val (hasSpace, encoding) = readEncoding(false)
      val standalone = readStandalone(hasSpace)
      space()
      (nextChar(): @switch) match {
        case '?' =>
          (nextChar(): @switch) match {
            case '>' => XmlDecl(minor, encoding, standalone)
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
      case Some(c) if c.isWhitespace =>
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
      case Some(c) if c.isWhitespace =>
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

  private def handleDecl(name: String): XmlEvent =
    name match {
      case "DOCTYPE" =>
        assert(_.isWhitespace, "XML [28]: space is expected after DOCTYPE")
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
            XmlDoctype(name, docname, systemid)
          case '[' =>
            skipInternalDTD()
            XmlDoctype(name, docname, systemid)
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
      case Some(PIToken(name)) =>
        skipPI()
        scanPrologToken2()
      case Some(StartToken(name)) =>
        readElement(name)
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
      EndTag(name)
    case StartTag(_, _, _) =>
      readContent()
    case EndTag(_) =>
      readCharData()
    case XmlString(_, _) =>
      readCharData()
    case EndDocument =>
      throw new NoSuchElementException
    case ExpectAttributes(name, _) =>
      completeStartTag(name)
    case ExpectAttributeValue(name, _, _) =>
      completeStartTag(name)
    case ExpectNodes =>
      readContent()
  }

}

private object XmlPullParser {

  val xmlNSURI = new URI("http://www.w3.org/XML/1998/namespace")

  val valueDelimiters = " \t\r\n<&"

}
