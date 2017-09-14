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
package dtd

import parser._

import scala.annotation.{
  tailrec,
  switch
}

import scala.io.Source

import scala.collection.immutable.VectorBuilder

trait DTDParser {
  this: XmlPullParser =>

  import XmlUtils._

  private var parameterEntities = Map.empty[String, PEDef]

  @tailrec
  final protected def readSubset(int: Boolean, acc: VectorBuilder[DTDEvent]): Seq[DTDEvent] =
    peekChar() match {
      case Some('%') =>
        // PE reference
        ???
      case Some('<') =>
        // a markup declaration
        readMarkupToken() match {
          case DeclToken("ELEMENT", l, c) =>
            acc += readElementDecl(l, c)
            readSubset(int, acc)
          case DeclToken("ATTLIST", l, c) =>
            acc += readAttListDecl(l, c)
            readSubset(int, acc)
          case DeclToken("ENTITY", l, c) =>
            space1("71", "expected space after 'ENTITY' declaration")
            peekChar() match {
              case Some('%') =>
                nextChar()
                space1("72", "expected space before parameter 'ENTITY' definition")
                val (n, d) = readPEDecl()
                parameterEntities = parameterEntities.updated(n, d)
                readSubset(int, acc)
              case _ =>
                acc += readGEDecl(l, c)
                readSubset(int, acc)
            }
          case DeclToken("NOTATION", l, c) =>
            space1("82", "expected space after 'NOTATION' declaration")
            val name = readNCName()
            space1("82", "expected space after 'NOTATION' name")
            readExternalID(true, true) match {
              case Some(externalid) =>
                space()
                accept('>', "82", "missing closing '>'")
                acc += XmlNotationDecl(name, externalid)(l, c)
                readSubset(int, acc)
              case None =>
                fail("82", "expected ExternalID or PublicID")
            }
          case DeclToken(n, l, c) =>
            fail("29", f"unexpected declaration '$n'")
          case CommentToken(l, c) =>
            skipComment(l, c)
            readSubset(int, acc)
          case PIToken(target, l, c) =>
            val body = readPIBody()
            acc += XmlPI(target, body)(l, c)
            readSubset(int, acc)
          case SectionToken(Left("INCLUDE"), l, c) if !int =>
            ???
          case SectionToken(Left("IGNORE"), l, c) if !int =>
            ???
          case SectionToken(Right(name), l, c) if !int =>
            parameterEntities.get(name) match {
              case Some(EntityValue(value)) =>
                value.map(_.render).mkString("", "", "").trim match {
                  case "INCLUDE" =>
                    ???
                  case "IGNORE" =>
                    ???
                  case _ =>
                    fail("61", "expected 'INCLUDE' or 'IGNORE'")
                }
              case _ =>
                fail("61", "expected 'INCLUDE' or 'IGNORE'")
            }
          case t =>
            fail("29", f"unexpected token type $t")
        }
      case Some(c) if isXmlWhitespace(c) =>
        space()
        readSubset(int, acc)
      case Some(']') =>
        nextChar()
        space()
        accept('>', "28", "unclosed doctype element")
        acc.result
      case None =>
        fail("28", "unexpected end of input")
      case Some(c) =>
        fail("28b", f"unexpected character '$c'")
    }

  private def readElementDecl(l: Int, c: Int): XmlElementDecl = {
    space1("45", "expected space after 'ELEMENT' declaration")
    val name = readQName()
    space1("45", "expected space after 'ELEMENT' name")
    val elt = (nextChar(): @switch) match {
      case '(' =>
        space()
        peekChar() match {
          case Some('#') =>
            // mixed
            val mixed = readMixed()
            XmlElementDecl(name, mixed)(l, c)
          case Some(_) =>
            val children = readChildren()
            XmlElementDecl(name, children)(l, c)
          case None =>
            fail("46", "unexpected end of input")
        }
      case 'A' =>
        if (read("NY") == 2) {
          XmlElementDecl(name, ANY)(l, c)
        } else {
          fail("46", "unexpected ELEMENT content specification")
        }
      case 'E' =>
        if (read("MPTY") == 4) {
          XmlElementDecl(name, EMPTY)(l, c)
        } else {
          fail("46", "unexpected ELEMENT content specification")
        }
      case _ =>
        fail("46", "unexpected ELEMENT content specification")
    }
    space()
    accept('>', "45", "missing closing '>'")
    elt
  }

  @tailrec
  private def readChoices(acc: VectorBuilder[Cp]): Seq[Cp] = {
    val cp = readCp()
    acc += cp
    space()
    (nextChar(): @switch) match {
      case '|' =>
        readChoices(acc)
      case ')' =>
        acc.result()
      case _ =>
        fail("49", "unexpected character")
    }
  }

  @tailrec
  private def readSeq(acc: VectorBuilder[Cp]): Seq[Cp] = {
    val cp = readCp()
    acc += cp
    space()
    (nextChar(): @switch) match {
      case ',' =>
        readSeq(acc)
      case ')' =>
        acc.result()
      case _ =>
        fail("50", "unexpected character")
    }
  }

  private def readChoiceOrSeq(): Content = {
    val fst = readCp()
    space()
    (nextChar(): @switch) match {
      case '|' =>
        space()
        val choices = readChoices(new VectorBuilder += fst)
        val m = readModifier()
        space()
        Choice(choices, m)
      case ',' =>
        space()
        val seq = readSeq(new VectorBuilder += fst)
        val m = readModifier()
        space()
        Sequence(seq, m)
      case ')' =>
        val m = readModifier()
        space()
        Sequence(Seq(fst), m)
    }
  }

  private def readModifier(): Option[Modifier] = peekChar() match {
    case Some('*') =>
      nextChar()
      Some(Star)
    case Some('+') =>
      nextChar()
      Some(Plus)
    case Some('?') =>
      nextChar()
      Some(Optional)
    case _ =>
      None
  }

  private def readCp(): Cp = peekChar() match {
    case Some('(') =>
      nextChar()
      readChoiceOrSeq()
    case Some(c) if isNCNameStart(c) =>
      val n = readQName()
      val m = readModifier()
      Name(n, m)
    case Some(_) =>
      fail("48", "unexpected character")
    case None =>
      fail("48", "unexpected end of input")
  }

  private def readChildren(): Children = {
    val content = readChoiceOrSeq()
    Children(content)
  }

  private def readMixed(): Mixed =
    if (read("#PCDATA") == 7) {
      @tailrec
      def readNames(acc: VectorBuilder[QName]): Seq[QName] = peekChar() match {
        case Some('|') =>
          nextChar()
          space()
          val n = readQName()
          space()
          readNames(acc += n)
        case _ =>
          acc.result()
      }
      space()
      val names = readNames(new VectorBuilder)
      accept(')', "51", "missing closing parenthesis")
      if (names.size > 0)
        accept('*', "51", "missing repetition star")
      else if (peekChar() == Some('*'))
        nextChar()
      Mixed(names)
    } else {
      fail("51", "expected '#PCDATA'")
    }

  private def readAttListDecl(l: Int, c: Int): XmlAttListDecl = {
    space1("52", "expected space after 'ATTLIST' declaration")
    val name = readQName()
    val s = space()
    @tailrec
    def readDefs(hasSpace: Boolean, acc: VectorBuilder[AttDef]): Seq[AttDef] =
      peekChar() match {
        case Some('>') =>
          nextChar()
          acc.result()
        case Some(c) if isNCNameStart(c) =>
          if (hasSpace) {
            val name = readQName()
            space1("53", "expected space after attribute name")
            val tpe = readAttType()
            space1("53", "expected space after attribute type")
            val dflt = readDefaultDecl()
            acc += AttDef(name, tpe, dflt)
            val s = space()
            readDefs(s, acc)
          } else {
            fail("53", "expected space before attribute definition")
          }
        case Some(_) =>
          fail("53", "unexpected character")
        case None =>
          fail("53", "unexpected end of input")
      }
    val defs = readDefs(s, new VectorBuilder)
    XmlAttListDecl(name, defs)(l, c)
  }

  private def readAttType(): AttType = peekChar() match {
    case Some('(') =>
      @tailrec
      def readNMTokens(acc: VectorBuilder[String]): Seq[String] = {
        val token = readQName().render
        acc += token
        space()
        peekChar() match {
          case Some('|') =>
            nextChar()
            space()
            readNMTokens(acc)
          case _ =>
            acc.result()
        }
      }
      nextChar()
      val nmtokens = readNMTokens(new VectorBuilder)
      space()
      accept(')', "59", "expected closing parenthesis after enumeration")
      ENUMType(nmtokens)
    case Some(c) if isNCNameStart(c) =>
      readNCName() match {
        case "CDATA"    => CDATAType
        case "ID"       => IDType
        case "IDREF"    => IDREFType
        case "IDREFS"   => IDREFSType
        case "ENTITY"   => ENTITYType
        case "ENTITIES" => ENTITIESType
        case "NMTOKEN"  => NMTOKENType
        case "NMTOKENS" => NMTOKENSType
        case "NOTATION" =>
          @tailrec
          def readNames(acc: VectorBuilder[String]): Seq[String] = {
            val n = readNCName()
            acc += n
            space()
            peekChar() match {
              case Some('|') =>
                nextChar()
                space()
                readNames(acc)
              case _ =>
                acc.result()
            }
          }
          space1("58", "expected space after 'NOTATION' type")
          accept('(', "58", "expected opening parenthesis before notations")
          val names = readNames(new VectorBuilder)
          space()
          accept(')', "58", "expected closing parenthesis after notations")
          NOTATIONType(names)
        case n =>
          fail("56", f"unexpexted attribute type '$n'")
      }
    case Some(_) =>
      fail("54", "unexpected character")
    case None =>
      fail("54", "unexpected end of input")
  }

  private def readDefaultDecl(): AttDefault = {
    def readFixed() = {
      val delimiter = assert(c => c == '"' || c == '\'', "10", "single or double quote expected around attribute value")
      val value = readAttributeValue(Some(delimiter), new StringBuilder, new VectorBuilder, line, column)
      FIXED(value)
    }
    peekChar() match {
      case Some('#') =>
        nextChar()
        readNCName() match {
          case "REQUIRED" => REQUIRED
          case "IMPLIED"  => IMPLIED
          case "FIXED" =>
            space1("60", "expected space after 'FIXED' default value")
            readFixed()
        }
      case Some(_) =>
        readFixed()
      case None =>
        fail("60", "unexpected end of input")
    }
  }

  private def readGEDecl(l: Int, c: Int): XmlGEDecl = {
    val n = readNCName()
    space1("71", "expected space after 'ENTITY' name")
    peekChar() match {
      case Some(d @ ('\'' | '"')) =>
        val v = readEntityValue(d)
        space()
        accept('>', "71", "missing closing '>'")
        XmlGEDecl(n, Left(v))(l, c)
      case _ =>
        readExternalID(true, false) match {
          case Some(externalid) =>
            val s = space()
            (nextChar(): @switch) match {
              case '>' =>
                XmlGEDecl(n, Right(externalid -> None))(l, c)
              case 'N' if s =>
                val data = read("DATA")
                if (data == 4) {
                  space1("76", "expected space before 'NDATA' name")
                  val ndata = readNCName()
                  space()
                  accept('>', "71", "missing closing '>'")
                  XmlGEDecl(n, Right(externalid -> Some(ndata)))(l, c)
                } else {
                  fail("76", "expected 'DATA'")
                }
              case _ =>
                fail("76", "expected 'DATA'")
            }
          case None =>
            fail("73", "expected ExternalID")
        }
    }
  }

  private def readPEDecl(): (String, PEDef) = {
    val n = readNCName()
    space1("72", "expected space after 'ENTITY' name")
    peekChar() match {
      case Some(d @ ('\'' | '"')) =>
        val v = readEntityValue(d)
        space()
        accept('>', "72", "missing closing '>'")
        (n, EntityValue(v))
      case _ =>
        readExternalID(true, false) match {
          case Some(externalid) =>
            accept('>', "72", "missing closing '>'")
            (n, ExternalPEDef(externalid))
          case None =>
            fail("74", "expected ExternalID")
        }
    }
  }

  private def readEntityValue(delimiter: Char): Seq[XmlTexty] = {
    @tailrec
    def loop(acc: VectorBuilder[XmlTexty]): Seq[XmlTexty] =
      peekChar() match {
        case Some('%') =>
          ???
        case Some('&') =>
          val l = line
          val c = column
          nextChar()
          peekChar() match {
            case Some('#') =>
              nextChar()
              val n = readCharRef()
              acc += XmlCharRef(n)(l, c)
              loop(acc)
            case _ =>
              val v = readNamedEntity()
              acc += XmlEntityRef(v)(l, c)
              loop(acc)
          }
        case Some(d) if d == delimiter =>
          nextChar()
          acc.result()
        case Some(_) =>
          val l = line
          val c = column
          val s = untilChar(c => c == '%' || c == '&' || c == 'd', new StringBuilder).toString
          acc += XmlString(s, false)(l, c)
          loop(acc)
        case None =>
          fail("9", "unexpected end of input")
      }
    loop(new VectorBuilder)
  }

  private def readExtSubsetDecl() =
    ???

}
