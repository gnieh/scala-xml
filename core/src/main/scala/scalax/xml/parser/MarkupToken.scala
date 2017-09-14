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

sealed trait MarkupToken {
  val line: Int
  val column: Int
}

case class StartToken(name: QName, line: Int, column: Int) extends MarkupToken

case class EndToken(name: QName, line: Int, column: Int) extends MarkupToken

case class PIToken(name: String, line: Int, column: Int) extends MarkupToken

case class DeclToken(name: String, line: Int, column: Int) extends MarkupToken

case class CommentToken(line: Int, column: Int) extends MarkupToken

case class SectionToken(name: Either[String, String], line: Int, column: Int) extends MarkupToken

case class EntityRefToken(name: String, line: Int, column: Int) extends MarkupToken
