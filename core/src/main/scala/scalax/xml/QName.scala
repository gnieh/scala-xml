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

import java.net.URI

case class QName(prefix: Option[String], local: String, uri: Option[URI]) {

  def render: String = prefix match {
    case Some(prefix) => f"$prefix:$local"
    case None         => local
  }

  def isResolved = !prefix.isDefined || uri.isDefined

  override def equals(o: Any): Boolean = o match {
    case QName(tprefix, tlocal, turi) =>
      if (isResolved)
        local == tlocal && uri.map(_.toString) == turi.map(_.toString)
      else
        local == tlocal && prefix == tprefix
    case _ =>
      false
  }

  override def hashCode: Int =
    if (isResolved)
      (17 + local.hashCode) * 31 + uri.map(_.toString).hashCode
    else
      (17 + local.hashCode) * 31 + prefix.hashCode

}

object QName {

  def apply(n: String): QName =
    QName(None, n, None)

}
