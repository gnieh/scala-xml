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

import dom._

import scala.annotation.tailrec

import scala.io.{
  Source,
  Codec
}

import scala.collection.immutable.VectorBuilder

import scala.reflect.classTag

import java.io.File

import java.net.URI

class DOMParser private (validate: Boolean, partial: Boolean, private var parts: Seq[Source], private var args: Seq[Any]) {

  import DOMParser._

  def predefinedEntities: Map[String, String] = predefEntities

  private var entities = Map.empty[String, NamedEntityBody]

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

  // stack of namespaces used for resolution
  private var namespaces: List[Map[String, URI]] = Nil

  private var stack: List[StartTag] = Nil

  private var elements: List[VectorBuilder[XmlNode]] = List(new VectorBuilder)

  private val partialAttributes = new VectorBuilder[Attribute]

  private def fail(line: Int, column: Int, error: XmlError, msg: String) =
    throw XmlException(line, column, error, msg)

  /** Parses the input source and returns the parsed document. */
  def parseDocument(): Document = {
    // force parsing if not done yet
    val r = root
    Document(version, encoding, standalone, r)
  }

  /** Parses the input source and returns the parsed root element. */
  def parse(): XmlNode = root

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
        // push declared namespaces to resolve children
        val localNS = attrs.foldLeft(Map.empty[String, URI]) {
          case (acc, Attribute(QName(None, "xmlns", _), seq)) =>
            val v = stringify(seq)
            if (v.isEmpty)
              fail(evt.line, evt.column, NSCNoPrefixUndeclaring, "default namespace cannot be undeclared")
            else
              acc.updated("", new URI(v))
          case (acc, Attribute(QName(Some("xmlns"), p, _), seq)) =>
            val v = stringify(seq)
            if (v.isEmpty)
              fail(evt.line, evt.column, NSCNoPrefixUndeclaring, f"namespace '$p' cannot be undeclared")
            else
              acc.updated(p, new URI(v))
          case (acc, _) =>
            acc
        }
        namespaces ::= localNS
        // reinitialize attribute list builder for next element
        partialAttributes.clear()
      case EndTag(ename) =>
        (stack, elements, namespaces) match {
          case (StartTag(sname, attributes, _) :: restStack, content :: (restElements @ builder :: _), _ :: restNamespaces) if ename == sname =>
            val attrs = attributes.foldLeft(Map.empty[QName, String]) {
              case (acc, Attribute(name @ QName(None, "xmlns", _), seq)) =>
                val v = stringify(seq)
                acc.updated(name, v)
              case (acc, Attribute(name @ QName(Some("xmlns"), p, _), seq)) =>
                val v = stringify(seq)
                acc.updated(name, v)
              case (acc, attr @ Attribute(name, value)) =>
                val resolved = resolveQName(name, false, evt.line, evt.column)
                if (acc.contains(resolved))
                  fail(evt.line, evt.column, NSCAttributesUnique, f"duplicate attribite with name $name")
                acc.updated(resolved, stringify(value))
            }
            builder += Elem(resolveQName(sname, true, evt.line, evt.column), attrs, content.result())
            stack = restStack
            elements = restElements
            namespaces = restNamespaces

          case (StartTag(sname, attributes, _) :: _, _, _) =>
            fail(evt.line, evt.column, WFCElementTypeMatch, f"expected closing tag '$sname' but got closing tag '$ename'")
          case _ =>
            throw new IllegalStateException
        }
      case XmlString(text, cdata) =>
        elements match {
          case builder :: _ =>
            if (cdata)
              builder += CDATA(text)
            else
              builder += Text(text)
          case _ =>
            throw new IllegalStateException
        }
      case evt @ XmlCharRef(n) =>
        elements match {
          case builder :: _ =>
            if (validate)
              builder += Text(stringify(Seq(evt)))
            else
              builder += CharRef(n)
          case _ =>
            throw new IllegalStateException
        }
      case evt @ XmlEntitiyRef(n) =>
        elements match {
          case builder :: _ =>
            if (validate)
              builder += Text(stringify(Seq(evt)))
            else
              builder += EntityRef(n)
          case _ =>
            throw new IllegalStateException
        }
      case XmlPI(target, body) =>
        elements match {
          case builder :: _ =>
            builder += PI(target, body)

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
            partialAttributes += Attribute(name, Seq(XmlString(arg.toString, false)(evt.line, evt.column)))
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
          case e @ Elem(_, _, _) => e
        }
        roots match {
          case Vector(root) => root
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

  @tailrec
  private def getNS(ns: String, namespaces: List[Map[String, URI]]): Option[URI] =
    namespaces match {
      case namespace :: namespaces =>
        namespace.get(ns) match {
          case None => getNS(ns, namespaces)
          case uri  => uri
        }
      case Nil =>
        None
    }

  private def resolveQName(name: QName, applyDefault: Boolean, l: Int, c: Int): QName =
    if (validate)
      name match {
        case QName(None, _, _) if !applyDefault =>
          name
        case QName(None, n, _) =>
          getNS("", namespaces) match {
            case Some(uri) => QName(None, n, Some(uri))
            case None      => name
          }
        case QName(Some(ns), n, _) =>
          getNS(ns, namespaces) match {
            case Some(uri)                          => QName(Some(ns), n, Some(uri))
            case None if ns.equalsIgnoreCase("xml") => QName(Some(ns), n, Some(xmlNSURI))
            case None                               => fail(l, c, NSCPrefixDeclared, f"undeclared namespace $ns")
          }
      }
    else
      name

  private def parsedEntitiy(name: String, l: Int, c: Int): String =
    entities.get(name) match {
      case Some(SourceNamedEntitiy(src)) =>
        entities = entities.updated(name, NEBlackHole)
        // TODO read content
        name
      case Some(NEBlackHole) =>
        fail(l, c, WFCNoRecursion, f"entity $name is recursive")
      case None =>
        predefEntities.get(name) match {
          case Some(s) =>
            s
          case None =>
            fail(l, c, WFCEntityDeclared, f"entitiy $name is not declared")
        }
    }

  private def stringify(seq: Seq[XmlTexty]): String = {
    val sb = new StringBuilder
    for (evt <- seq) evt match {
      case XmlString(s, _) =>
        sb.append(s)
      case XmlEntitiyRef(n) =>
        if (validate)
          sb.append(parsedEntitiy(n, evt.line, evt.column))
        else
          sb.append('&').append(n).append(';')
      case XmlCharRef(n) =>
        if (validate)
          sb.append(Character.toChars(n))
        else
          sb.append("&#").append(n.toHexString).append(';')
    }
    sb.toString
  }

}

object DOMParser {

  def fromString(str: String): DOMParser =
    new DOMParser(true, false, Seq(Source.fromString(str)), Seq.empty)

  def fromFile(f: String)(implicit codec: Codec): DOMParser =
    new DOMParser(true, false, Seq(Source.fromFile(f)), Seq.empty)

  def fromFile(f: File)(implicit codec: Codec): DOMParser =
    new DOMParser(true, false, Seq(Source.fromFile(f)), Seq.empty)

  def fromParts(parts: Seq[Source], args: Seq[Any]): DOMParser =
    new DOMParser(true, true, parts, args)

  private[parser] val AttributesTag = classTag[Attributes]

  private[parser] val NodesTag = classTag[Seq[XmlNode]]

  private[parser] val predefEntities = Map(
    "lt" -> "<",
    "gt" -> ">",
    "amp" -> "&",
    "apos" -> "'",
    "quot" -> "\"")

  private[parser] val xmlNSURI = new URI("http://www.w3.org/XML/1998/namespace")

}
