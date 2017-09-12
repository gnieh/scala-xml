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

/** Represents a namespace constraint as defined in the XML namespaces recommendation. */
sealed abstract class NSError(name: String) extends XmlError(name)
case object NSCPrefixDeclared extends NSError("[NSC: Prefix Declared]")
case object NSCNoPrefixUndeclaring extends NSError("[NSC: No Prefix Undeclaring]")
case object NSCAttributesUnique extends NSError("[NSC: Attributes Unique]")

/** Represents a Well-formedness constraint as defined in the XML specification. */
sealed abstract class WFError(name: String) extends XmlError(name)
case object WFCElementTypeMatch extends WFError("[WFC: Element Type Match]")
case object WFCEntityDeclared extends WFError("[WFC: Entity Declared]")
case object WFCNoRecursion extends WFError("[WFC: No Recursion]")
