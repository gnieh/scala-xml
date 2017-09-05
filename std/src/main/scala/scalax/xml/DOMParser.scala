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

import parser._
import tree._

import scala.annotation.tailrec

import scala.io.{
  Source,
  Codec
}

import scala.collection.immutable.VectorBuilder

import scala.reflect.classTag

import java.io.File

import java.net.URI

class DOMParser private (partial: Boolean, private var parts: Seq[Source], private var args: Seq[Any]) {

  import DOMParser._

  private val parser = parts match {
    case Seq(part) =>
      parts = Nil
      new XmlPullParser(part, false)
    case Seq(fst, rest @ _*) =>
      parts = rest
      new XmlPullParser(fst, partial)
    case Seq() =>
      throw new IllegalArgumentException("at least one part must be provided")
  }

  private var stack: List[StartTag] = Nil

  private var elements: List[VectorBuilder[XmlTree]] = List(new VectorBuilder)

  private val partialAttributes = new VectorBuilder[Attr]

  private def fail(line: Int, column: Int, error: XmlError, msg: String) =
    throw XmlException(new LineColumnPosition(line, column), error, msg)

  /** Parses the input source and returns the parsed document. */
  def parseDocument(): Document = {
    // force parsing if not done yet
    val r = root
    Document(version, encoding, standalone, r)
  }

  /** Parses the input source and returns the parsed root element. */
  def parse(): XmlTree = root

  private var version: Option[String] = None

  private var encoding: Option[String] = None

  private var standalone: Option[Boolean] = None

  private lazy val root = {
    for (evt <- parser) evt match {
      case XmlDecl(v, e, s) =>
        version = Some(v)
        encoding = e
        standalone = s
      case evt @ StartTag(_, attributes, _) =>
        // finalize attribute list
        partialAttributes ++= attributes
        val attrs = partialAttributes.result()
        // push finalized start tag on the stack
        stack ::= evt.copy(attributes = attrs)(evt.line, evt.column)
        // initialize children list
        elements ::= new VectorBuilder
        // reinitialize attribute list builder for next element
        partialAttributes.clear()
      case EndTag(ename) =>
        (stack, elements) match {
          case (StartTag(sname, attributes, _) :: restStack, content :: (restElements @ builder :: _)) if ename == sname =>
            val attrs = attributes.map {
              case Attr(name, seq) =>
                Tree(Attribute(name), domify(seq))
            }
            builder += Tree(Elem(sname, attrs), content.result())
            stack = restStack
            elements = restElements

          case (StartTag(sname, attributes, _) :: _, _) =>
            fail(evt.line, evt.column, WFCElementTypeMatch, f"expected closing tag '$sname' but got closing tag '$ename'")
          case _ =>
            throw new IllegalStateException
        }
      case XmlString(text, cdata) =>
        elements match {
          case builder :: _ =>
            if (cdata)
              builder += Tree(CDATA(text))
            else
              builder += Tree(Text(text))
          case _ =>
            throw new IllegalStateException
        }
      case evt @ XmlCharRef(n) =>
        elements match {
          case builder :: _ =>
            builder += Tree(CharRef(n))
          case _ =>
            throw new IllegalStateException
        }
      case evt @ XmlEntitiyRef(n) =>
        elements match {
          case builder :: _ =>
            builder += Tree(EntityRef(n))
          case _ =>
            throw new IllegalStateException
        }
      case XmlPI(target, body) =>
        elements match {
          case builder :: _ =>
            builder += Tree(PI(target, body))

          case _ =>
            throw new IllegalStateException
        }
      case ExpectAttributes(_, attrs) if partial =>
        partialAttributes ++= attrs
        args match {
          case Seq(AttributesTag(arg), rest @ _*) =>
            args = rest
            partialAttributes ++= arg
            feedNextPart()
          case _ =>
            throw new IllegalArgumentException
        }
      case ExpectAttributeValue(_, attrs, name) if partial =>
        partialAttributes ++= attrs
        args match {
          case Seq(null, rest @ _*) =>
            // null value, just skip this attribute
            args == rest
            feedNextPart()
          case Seq(arg, rest @ _*) =>
            args = rest
            partialAttributes += Attr(name, Seq(XmlString(arg.toString, false)(evt.line, evt.column)))
            feedNextPart()
          case _ =>
            throw new IllegalArgumentException
        }
      case ExpectNodes() if partial =>
        args match {
          case Seq(NodesTag(arg), rest @ _*) =>
            args = rest
            elements match {
              case builder :: _ =>
                builder ++= arg
                feedNextPart()
              case _ =>
                throw new IllegalStateException
            }
          case _ =>
            throw new IllegalArgumentException
        }

      case _ =>
      // do nothing for other events
    }
    (stack, elements) match {
      case (Nil, List(builder)) =>
        val roots = builder.result().collect {
          case e @ Tree(Elem(_, _), _) => e
        }
        roots match {
          case Vector(root) => root
          case Vector()     => fail(1, 1, XmlSyntax("1"), "missing root element")
          case _            => fail(1, 1, XmlSyntax("1"), "several root elements")
        }
      case ((evt @ StartTag(name, _, _)) :: _, _) =>
        fail(evt.line, evt.column, XmlSyntax("39"), f"unclosed element $name")
      case _ =>
        throw new IllegalStateException
    }
  }

  private def feedNextPart() =
    parts match {
      case Seq(part) =>
        parts = Seq()
        parser.partial = false
        parser.feed(part)
      case Seq(part, rest @ _*) =>
        parts = rest
        parser.feed(part)
      case _ =>
        throw new IllegalStateException
    }

  private def domify(seq: Seq[XmlTexty]): Seq[XmlTree] = seq.map {
    case XmlString(s, false) => Tree(Text(s))
    case XmlString(s, true)  => Tree(CDATA(s))
    case XmlCharRef(n)       => Tree(CharRef(n))
    case XmlEntitiyRef(n)    => Tree(EntityRef(n))
  }

}

object DOMParser {

  def fromString(str: String): DOMParser =
    new DOMParser(false, Seq(Source.fromString(str)), Seq.empty)

  def fromFile(f: String)(implicit codec: Codec): DOMParser =
    new DOMParser(false, Seq(Source.fromFile(f)), Seq.empty)

  def fromFile(f: File)(implicit codec: Codec): DOMParser =
    new DOMParser(false, Seq(Source.fromFile(f)), Seq.empty)

  def fromParts(parts: Seq[Source], args: Seq[Any]): DOMParser =
    new DOMParser(true, parts, args)

  def fromSource(s: Source): DOMParser =
    new DOMParser(false, Seq(s), Seq.empty)

  private[xml] val NodesTag = classTag[Seq[XmlTree]]

  private[xml] val AttributesTag = classTag[Seq[Attr]]

}
