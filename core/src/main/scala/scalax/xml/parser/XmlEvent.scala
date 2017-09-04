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

sealed trait XmlEvent {
  val line: Int
  val column: Int
}

sealed trait XmlTexty extends XmlEvent

case object StartDocument extends XmlEvent {
  val line = 1
  val column = 1
}

final case class XmlDecl(version: String, encoding: Option[String], standalone: Option[Boolean])(val line: Int, val column: Int) extends XmlEvent

final case class StartTag(name: QName, attributes: Attributes, isEmpty: Boolean)(val line: Int, val column: Int) extends XmlEvent

final case class XmlCharRef(value: Int)(val line: Int, val column: Int) extends XmlTexty

final case class XmlEntitiyRef(name: String)(val line: Int, val column: Int) extends XmlTexty

final case class XmlString(s: String, isCDATA: Boolean)(val line: Int, val column: Int) extends XmlTexty

final case class XmlDoctype(name: String, docname: String, systemid: Option[String])(val line: Int, val column: Int) extends XmlEvent

final case class EndTag(name: QName)(val line: Int, val column: Int) extends XmlEvent

final case class EndDocument()(val line: Int, val column: Int) extends XmlEvent

final case class ExpectAttributes(name: QName, attributes: Attributes)(val line: Int, val column: Int) extends XmlEvent

final case class ExpectAttributeValue(tname: QName, attributes: Attributes, aname: QName)(val line: Int, val column: Int) extends XmlEvent

final case class ExpectNodes()(val line: Int, val column: Int) extends XmlEvent
