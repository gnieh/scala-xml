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

import dtd._

sealed trait XmlEvent {
  val line: Int
  val column: Int
}

sealed trait XmlTexty extends XmlEvent {
  def render: String
}

case object StartDocument extends XmlEvent {
  val line = 1
  val column = 1
}

final case class XmlDecl(version: String, encoding: Option[String], standalone: Option[Boolean])(val line: Int, val column: Int) extends XmlEvent

final case class StartTag(name: QName, attributes: Seq[Attr], isEmpty: Boolean)(val line: Int, val column: Int) extends XmlEvent

final case class XmlCharRef(value: Int)(val line: Int, val column: Int) extends XmlTexty {
  def render = f"&#$value;"
}

final case class XmlEntityRef(name: String)(val line: Int, val column: Int) extends XmlTexty {
  def render = f"&$name;"
}

final case class XmlString(s: String, isCDATA: Boolean)(val line: Int, val column: Int) extends XmlTexty {
  def render = if (isCDATA) f"<!CDATA{$s]]>" else s
}

final case class XmlPI(target: String, content: String)(val line: Int, val column: Int) extends DTDEvent

final case class XmlDoctype(name: String, docname: String, externalid: Option[ExternalId], dtd: Option[Seq[DTDEvent]])(val line: Int, val column: Int) extends XmlEvent

final case class EndTag(name: QName)(val line: Int, val column: Int) extends XmlEvent

final case class EndDocument()(val line: Int, val column: Int) extends XmlEvent

final case class ExpectAttributes(name: QName, attributes: Seq[Attr])(val line: Int, val column: Int) extends XmlEvent

final case class ExpectAttributeValue(tname: QName, attributes: Seq[Attr], aname: QName)(val line: Int, val column: Int) extends XmlEvent

final case class ExpectNodes()(val line: Int, val column: Int) extends XmlEvent

sealed trait DTDEvent extends XmlEvent

final case class XmlElementDecl(name: QName, kind: Kind)(val line: Int, val column: Int) extends DTDEvent

final case class XmlAttListDecl(name: QName, defs: Seq[AttDef])(val line: Int, val column: Int) extends DTDEvent

final case class XmlNotationDecl(name: String, externalid: ExternalId)(val line: Int, val column: Int) extends DTDEvent

final case class XmlGEDecl(name: String, definition: Either[Seq[XmlTexty], (ExternalId, Option[String])])(val line: Int, val column: Int) extends DTDEvent
