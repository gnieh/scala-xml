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

final case class ElementDecl(name: String, kind: Kind) extends MarkupDecl

sealed trait Kind

case object EMPTY extends Kind

case object ANY extends Kind

final case class Children(content: Content) extends Kind

sealed trait Modifier

case object Optional extends Modifier

case object Star extends Modifier

case object Plus extends Modifier

sealed trait Cp {
  val modifier: Modifier
}

sealed trait Content extends Cp

final case class Choice(choices: Seq[Cp], modifier: Modifier) extends Content

final case class Sequence(seq: Seq[Cp], modifier: Modifier) extends Content

final case class Name(name: String, modifier: Modifier) extends Cp

sealed trait Mixed extends Kind

case object PCDATA extends Mixed

final case class Interspersed(names: Seq[String]) extends Mixed

final case class AttListDecl(name: String, attributes: Seq[AttDef]) extends MarkupDecl

final case class AttDef(name: String, tpe: AttType, default: AttDefault)

sealed trait AttType

case object CDATAType extends AttType

case object IDType extends AttType

case object IDREFType extends AttType

case object IDREFSType extends AttType

case object ENTITYType extends AttType

case object ENTITIESType extends AttType

case object NMTOKENType extends AttType

case object NMTOKENSType extends AttType

final case class NOTATIONType(names: Seq[Name]) extends AttType

final case class ENUMType(tokens: Seq[String]) extends AttType

sealed trait AttDefault

case object REQUIRED extends AttDefault

case object IMPLIED extends AttDefault

final case class FIXED(value: String) extends AttDefault

sealed trait EntityDecl extends MarkupDecl

final case class GEDecl(name: String, definition: EntityDef) extends EntityDecl

final case class PEDecl(name: String, definition: PEDef) extends EntityDecl

sealed trait EntityDef

sealed trait PEDef

final case class EntityValue(value: String) extends EntityDef with PEDef

final case class ExternalEntityDef(externalid: ExternalId, notation: Option[String]) extends EntityDef

final case class ExternalPEDef(externalid: ExternalId) extends PEDef

final case class NotationDecl(name: String, id: Either[ExternalId, PublicId]) extends MarkupDecl

final case class PublicId(id: String)
