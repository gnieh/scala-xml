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

import org.scalatest._

import better.files._

class ConformanceNotWFTest extends FlatSpec with Matchers {

  val ignored = ("core" / "src" / "test" / "resources" / "conformance" / "not-wf" / "sa" / "ignored.txt").lines.map(_.trim).filterNot(l => l.isEmpty || l.startsWith("#")).toSet

  for (f <- ("core" / "src" / "test" / "resources" / "conformance" / "not-wf" / "sa").list.filter(_.extension == Some(".xml"))) {
    val scope =
      if (ignored.contains(f.name))
        f"not-wf XML document $f" should "be parsed with error" ignore {
        }
      else
        f"not-wf XML document $f" should "be parsed with error" in {
          try {
            val parser = DOMParser.fromFile(f.toJava)
            an[ParserException] should be thrownBy parser.parse()
          } catch {
            case _: java.nio.charset.MalformedInputException =>
              import java.nio.charset.CodingErrorAction
              import scala.io.Codec

              implicit val codec = Codec("UTF-16")
              val parser = DOMParser.fromFile(f.toJava)
              an[ParserException] should be thrownBy parser.parse()
          }
        }
  }

}
