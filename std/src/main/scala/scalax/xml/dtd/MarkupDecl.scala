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
package dtd

sealed trait MarkupDecl

final case class ElementDecl(name: QName, kind: Kind) extends MarkupDecl

final case class AttListDecl(name: QName, attributes: Seq[AttDef]) extends MarkupDecl

sealed trait EntityDecl extends MarkupDecl

final case class GEDecl(name: String, definition: EntityDef) extends EntityDecl {
  val isParsed: Boolean = definition.isParsed
}

sealed trait EntityDef {
  val isParsed: Boolean
}

final case class EntityValue(value: Seq[XmlNode]) extends EntityDef {
  val isParsed: Boolean = true
}

final case class ExternalEntityDef(externalid: ExternalId, notation: Option[String]) extends EntityDef {
  val isParsed: Boolean = notation.isEmpty
}

private case object GEBeingDefined extends EntityDef {
  val isParsed: Boolean = true
}

final case class NotationDecl(name: String, id: ExternalId) extends MarkupDecl

final case class PIDecl(target: String, body: String) extends MarkupDecl
