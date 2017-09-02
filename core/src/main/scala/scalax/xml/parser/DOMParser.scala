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

import scala.io.Source

import scala.collection.immutable.VectorBuilder

import scala.reflect.classTag

class DOMParser private (partial: Boolean, private var parts: Seq[Source], private var args: Seq[Any]) {

  import DOMParser._

  private val parser = parts match {
    case Seq(fst, rest @ _*) =>
      parts = rest
      new XmlPullParser(fst, Map.empty, Map.empty, partial)
    case Seq() =>
      throw new Exception("at least one part must be provided")
  }

  private var stack: List[StartTag] = Nil

  private var elements: List[VectorBuilder[XmlNode]] = List(new VectorBuilder)

  private val partialAttributes = new VectorBuilder[Attribute]

  def parse(): XmlNode = {
    for (evt <- parser) evt match {
      case evt @ StartTag(_, _, _) =>
        stack ::= evt
        elements ::= new VectorBuilder
      case EndTag(ename) =>
        (stack, elements) match {
          case (StartTag(sname, attributes, _) :: restStack, content :: (restElements @ builder :: _)) if ename == sname =>
            partialAttributes ++= attributes
            val attrs = partialAttributes.result().foldLeft(Map.empty[QName, String]) {
              case (acc, attr @ Attribute(name, value)) =>
                if (acc.contains(name))
                  throw new Exception(f"[uniqattspec]: duplicate attribite with name $name")
                acc.updated(name, value)
            }
            builder += Elem(sname, attrs, content.result())
            stack = restStack
            elements = restElements
            partialAttributes.clear()

          case (StartTag(sname, attributes, _) :: _, _) =>
            throw new Exception(f"[GIMatch]: expected closing tag '$sname' but got closing tag '$ename'")
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
      case ExpectAttributes(_, attrs) if partial =>
        partialAttributes ++= attrs
        args match {
          case Seq(AttributesTag(arg), rest @ _*) =>
            args = rest
            partialAttributes ++= arg
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
          case _ =>
            throw new Exception("invalid arguments")
        }

      case ExpectNodes if partial =>
        args match {
          case Seq(NodesTag(arg), rest @ _*) =>
            args = rest
            elements match {
              case builder :: _ =>
                builder ++= arg
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
              case _ =>
                throw new IllegalStateException
            }
          case _ =>
            println(args)
            throw new Exception("invalid arguments")
        }

      case _ =>
      // do nothing for other events
    }
    (stack, elements) match {
      case (Nil, List(builder)) =>
        builder.result() match {
          case Vector(root) => root
          case _            => throw new Exception("XML [1]: several root elements")
        }
      case (StartTag(name, _, _) :: _, _) =>
        throw new Exception(f"XML [39]: unclosed element $name")
      case _ =>
        throw new IllegalStateException
    }
  }

}

object DOMParser {

  def fromString(str: String): DOMParser =
    new DOMParser(false, Seq(Source.fromString(str)), Seq.empty)

  def fromParts(parts: Seq[Source], args: Seq[Any]): DOMParser =
    new DOMParser(true, parts, args)

  private[parser] val AttributesTag = classTag[Attributes]

  private[parser] val NodesTag = classTag[Seq[XmlNode]]

}
