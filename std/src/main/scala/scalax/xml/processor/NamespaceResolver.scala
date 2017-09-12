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
package processor

import tree._

import java.net.URI

object NamespaceResolver extends XmlTransformer[Map[String, URI]] {

  private val xmlNSURI = Some(new URI("http://www.w3.org/XML/1998/namespace"))

  protected def updateState(previous: Map[String, URI], t: XmlTree): Map[String, URI] = t match {
    case Tree(Elem(_, attrs), _) =>
      attrs.foldLeft(previous)(updateNS)
    case _ =>
      previous
  }

  protected def transformNode(state: Map[String, URI], t: XmlTree): XmlNode = t match {
    case Tree(Elem(n, attrs), _) =>
      val attrs1 = attrs.map(transform(state, _))
      checkDuplicates(attrs1, Set())
      Elem(resolve(state, n, true), attrs1)
    case Tree(Attribute(n), _) =>
      Attribute(resolve(state, n, false))
    case Tree(n, _) =>
      n
  }

  private def resolve(env: Map[String, URI], name: QName, withDefault: Boolean): QName = name match {
    case QName(Some(pfx), local, _) =>
      env.get(pfx) match {
        case None => throw new XmlException(NoPosition, NSCPrefixDeclared, f"undeclared namespace $pfx")
        case uri  => name.copy(uri = uri)
      }
    case QName(None, _, _) if withDefault =>
      name.copy(uri = xmlNSURI)
    case _ =>
      name
  }

  private def checkDuplicates(attrs: Seq[XmlTree], seen: Set[QName]): Unit =
    attrs match {
      case Seq(Tree(Attribute(n), _), xs @ _*) =>
        if (seen.contains(n))
          throw new XmlException(NoPosition, NSCAttributesUnique, f"duplicate attribute with resolved name ${n.render}")
        else
          checkDuplicates(xs, seen + n)
      case Seq(_, xs @ _*) =>
        checkDuplicates(xs, seen)
      case _ =>

    }

  private def updateNS(env: Map[String, URI], attr: XmlTree): Map[String, URI] =
    attr match {
      case Tree(Attribute(QName(None, "xmlns", _)), children) =>
        val v = children.render(new StringBuilder).toString
        if (v.isEmpty)
          throw new XmlException(NoPosition, NSCNoPrefixUndeclaring, "undeclaring default namespace is not allowed")
        else
          env.updated("", new URI(v))
      case Tree(Attribute(QName(Some("xmlns"), name, _)), children) =>
        val v = children.render(new StringBuilder).toString
        if (v.isEmpty)
          throw new XmlException(NoPosition, NSCNoPrefixUndeclaring, f"undeclaring namespace $name is not allowed")
        else
          env.updated(name, new URI(v))
      case _ =>
        env
    }

}
