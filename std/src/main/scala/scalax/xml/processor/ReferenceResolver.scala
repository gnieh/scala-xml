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

object ReferenceResolver extends XmlTransformer[Map[String, String]] {

  protected def updateState(previous: Map[String, String], n: Tree[XmlNode]): Map[String, String] = previous

  protected def transformNode(entities: Map[String, String], t: Tree[XmlNode]): XmlNode = t match {
    case Tree(Elem(n, attrs), _) =>
      Elem(n, attrs.map(transform(entities, _)))
    case Tree(CharRef(n), _) =>
      Text(new String(Character.toChars(n)))
    case Tree(n @ EntityRef(name), _) =>
      entities.get(name) match {
        case Some(value) =>
          Text(value)
        case None =>
          throw new XmlException(NoPosition, WFCEntityDeclared, f"undeclared entity $n")
      }
    case Tree(n, _) =>
      n
  }

}
