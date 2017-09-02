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

class DOMParser(input: Source) {

  private val parser = new XmlPullParser(input, Map.empty, Map.empty, false)

  private var stack: List[StartTag] = Nil

  private var elements: List[VectorBuilder[XmlNode]] = List(new VectorBuilder)

  def parse(): XmlNode = {
    for (evt <- parser) evt match {
      case evt @ StartTag(_, _, _) =>
        stack ::= evt
        elements ::= new VectorBuilder
      case EndTag(ename) =>
        (stack, elements) match {
          case (StartTag(sname, attributes, _) :: restStack, content :: (restElements @ builder :: _)) if ename == sname =>
            val attrs = attributes.foldLeft(Map.empty[QName, String]) {
              case (acc, attr @ Attribute(name, value)) =>
                if (acc.contains(name))
                  throw new Exception(f"[uniqattspec]: duplicate attribite with name $name")
                acc.updated(name, value)
            }
            builder += Elem(sname, attrs, content.result())
            stack = restStack
            elements = restElements

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
    new DOMParser(Source.fromString(str))

}
