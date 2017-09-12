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
package scalax

import scala.io.Source

import java.net.URI

package object xml {

  import processor._
  import tree._

  val xmlPredefinedEntities = Map(
    "lt" -> "<",
    "gt" -> ">",
    "amp" -> "&",
    "apos" -> "'",
    "quot" -> "\"")

  def parse(s: Source, validate: Boolean): XmlTree =
    if (validate)
      ReferenceResolver.transform(xmlPredefinedEntities, NamespaceResolver.transform(Map(), DOMParser.fromSource(s).parse()))
    else
      DOMParser.fromSource(s).parse()

  implicit class XmlInterpolators(val sc: StringContext) extends AnyVal {

    def xml(args: Any*): Tree[XmlNode] =
      DOMParser.fromParts(sc.parts.map(Source.fromString(_)), args).parse()

    def xmldoc(args: Any*): Document =
      DOMParser.fromParts(sc.parts.map(Source.fromString(_)), args).parseDocument()

  }

  type XmlTree = Tree[XmlNode]

  implicit class XmlTreeOps(val t: XmlTree) extends AnyVal {

    def render(builder: StringBuilder): StringBuilder = t match {
      case Tree(Text(s), _) =>
        builder.append(s)
      case Tree(Elem(n, atts), Seq()) =>
        builder.append('<').append(n.render)
        for (att <- atts)
          att.render(builder)
        builder.append("/>")
      case Tree(Elem(n, atts), chldn) =>
        builder.append('<').append(n.render)
        for (att <- atts)
          att.render(builder)
        builder.append('>')
        for (chld <- chldn)
          chld.render(builder)
        builder.append("</").append(n.render).append('>')
      case Tree(Attribute(n), chldn) =>
        builder.append(' ').append(n.render).append("=\"")
        for (chld <- chldn)
          chld.render(builder)
        builder.append('"')
      case Tree(CharRef(n), _) =>
        builder.append("&#").append(n).append(';')
      case Tree(EntityRef(n), _) =>
        builder.append("&").append(n).append(';')
      case Tree(Comment(s), _) =>
        builder.append("<!--").append(s).append("-->")
      case Tree(CDATA(s), _) =>
        builder.append("<![CDATA[").append(s).append("]]>")
      case Tree(PI(tgt, content), _) =>
        builder.append("<?").append(tgt).append(content).append("?>")
    }

  }

  implicit class XmlTreesOps(val ts: Seq[XmlTree]) extends AnyVal {

    def render(sb: StringBuilder): StringBuilder =
      ts.foldLeft(sb)((sb, t) => t.render(sb))

  }
}
