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

sealed trait XmlEvent

case object StartDocument extends XmlEvent

final case class StartTag(name: QName, attributes: Attributes, isEmpty: Boolean) extends XmlEvent

final case class XmlString(s: String, isCDATA: Boolean) extends XmlEvent

final case class XmlDoctype(name: String, docname: String, systemid: Option[String]) extends XmlEvent

final case class EndTag(name: QName) extends XmlEvent

case object EndDocument extends XmlEvent

final case class ExpectAttributes(name: QName, attributes: Attributes) extends XmlEvent

final case class ExpectAttributeValue(tname: QName, attributes: Attributes, aname: QName) extends XmlEvent

case object ExpectNodes extends XmlEvent
