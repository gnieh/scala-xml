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

import tree._

sealed abstract class XmlNode {

  def attributes: Option[Seq[XmlTree]] = None

  val isText: Boolean = false

  val isCharRef: Boolean = false

  val isEntityRef: Boolean = false

  val isComment: Boolean = false

  val isCDATA = false

  val isPI: Boolean = false

  val isElem: Boolean = false

  val isAttr: Boolean = false

  def text: Option[String] = None

  def qname: Option[QName] = None

  def comment: Option[String] = None

  def entityRef: Option[String] = None

  def charRef: Option[Int] = None

  def cdata: Option[String] = None

  def piTarget: Option[String] = None

  def piContent: Option[String] = None

}

case class Document(
    version: Option[String],
    encoding: Option[String],
    standalone: Option[Boolean],
    root: Tree[XmlNode])

case class Elem(name: QName, attrs: Seq[XmlTree]) extends XmlNode {
  override def attributes = Some(attrs)
  override val isElem = true
  override def qname = Some(name)
}

case class Attribute(name: QName) extends XmlNode {
  override val isAttr = true
  override def qname = Some(name)
}

case class Comment(content: String) extends XmlNode {
  override val isComment = true
  override def comment = Some(content)
}

case class Text(content: String) extends XmlNode {
  override val isText = true
  override def text = Some(content)
}

case class EntityRef(name: String) extends XmlNode {
  override val isEntityRef = true
  override def entityRef = Some(name)
}

case class CharRef(value: Int) extends XmlNode {
  override val isCharRef = true
  override def charRef = Some(value)
}

case class CDATA(content: String) extends XmlNode {
  override val isCDATA = true
  override def cdata = Some(content)
}

case class PI(target: String, content: String) extends XmlNode {
  override val isPI = true
  override def piTarget = Some(target)
  override def piContent = Some(content)
}
