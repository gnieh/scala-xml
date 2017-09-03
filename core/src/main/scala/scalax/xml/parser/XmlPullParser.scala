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

  // characters under 0x10000 are in UTF-16 with surrogate blocks
  private def isValid(c: Int): Boolean =
    (0x10000 <= c && c <= 0x10ffff) || (c < 0x10000 && c != 0xfffe && c != 0xffff)

  private def isSpace(c: Char): Boolean =
    c == ' ' || c == '\t' || c == '\r' || c == '\n'

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
        readQuoted()
      case "PUBLIC" =>
        readQuoted()
        assert(isSpace(_), "XML [12]: space required after PubidLiteral")
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
  def readCharRef(): Char = {
    def postlude(n: Int) =
      (nextChar(): @switch) match {
        case ';' =>
          if (isValid(n))
            if (n < 0x10000) {
              n.toChar
            } else {
              // get the surrogate characters
              val Array(c, cs @ _*) = Character.toChars(n)
              buffer = cs.foldRight(buffer) { (c, buffer) =>
                c +: buffer
              }
              c
            }
          else {
            fail("XML [2]: invalid character")
          }
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
      case Some(c) if isSpace(c) =>
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

          StartTag(resolved, proper, isEmpty)(l, c)
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
          case CommentToken(l, c) =>
            readCharData()
          case DeclToken(n, l, c) =>
            fail(f"Unexpected declaration '$n'")
          case CDataToken(l, c) =>
            val body = readCDATABody(new StringBuilder)
            XmlString(body, true)(l, c)
          case EndToken(name, l, c) =>
            EndTag(name)(l, c)
          case StartToken(name, l, c) =>
            completeStartTag(name, l, c)
          case PIToken(target, l, c) =>
            skipPI()
            readCharData()
          case t =>
            fail(f"XML [43]: unexpected token $t")
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
    untilChar(c => c == '<' || c == '&' || c == '\r', sb)
    peekChar() match {
      case Some('<') => XmlString(sb.toString, false)(l, c)
      case None      => XmlString(sb.toString, false)(l, c)
      case Some('&') =>
        nextChar()
        peekChar() match {
          case Some('#') =>
            nextChar()
            sb.append(readCharRef())
            slowPath(sb, l, c)
          case _ =>
            val v = readNamedEntity()
            sb.append(v)
            slowPath(sb, l, c)
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
    scanMisc() match {
      case None =>
        fail("XML [22]: unexpected end of input")
      case Some(PIToken(name, l, c)) if name.equalsIgnoreCase("xml") =>
        handleXmlDecl(l, c)
      case Some(PIToken(name, l, c)) =>
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

  @tailrec
  private def scanPrologToken1(): XmlEvent = {
    scanMisc() match {
      case None =>
        fail("XML [22]: unexpected end of input")
      case Some(PIToken(name, l, c)) =>
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
            case '>' => XmlDecl(minor, encoding, standalone)(l, c)
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
      case Some(PIToken(name, l, c)) =>
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
      EndTag(name)(previousEvent.line, previousEvent.column)
    case StartTag(_, _, _) =>
      level += 1
      readContent()
    case EndTag(_) =>
      level -= 1
      readCharData()
    case XmlString(_, _) =>
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

  val xmlNSURI = new URI("http://www.w3.org/XML/1998/namespace")

  val valueDelimiters = " \t\r\n<&"

}
