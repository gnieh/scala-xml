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

import dom._

import org.scalatest._

class PartialParsingTest extends FlatSpec with Matchers {

  "an XML document expecting attributes" should "be correctly parsed if attribute sequence is provided" in {
    val attrs = Seq(Attribute(QName("a"), "value1"), Attribute(QName("b"), "value2"))
    val parsed = xml"<root $attrs/>"
    val expected = Elem(QName("root"), Map(QName("a") -> "value1", QName("b") -> "value2"), Seq.empty)

    parsed should be(expected)
  }

  it should "add provided attributes to the parsed ones" in {
    val attrs = Seq(Attribute(QName("a"), "value1"), Attribute(QName("b"), "value2"))
    val parsed = xml"""<root c="value3" $attrs d="value4"/>"""
    val expected = Elem(QName("root"), Map(QName("a") -> "value1", QName("b") -> "value2", QName("c") -> "value3", QName("d") -> "value4"), Seq.empty)

    parsed should be(expected)
  }

  it should "not parse if provided attributes are not of the correct type" in {
    val attrs = 1
    an[Exception] should be thrownBy xml"<root $attrs/>"
  }

  "an XML document expecting an attribute value" should "be correctly parsed if a non-null value is provided" in {
    val attrValue1 = 1
    val attrValue2 = "v"
    val parsed = xml"""<root a="0" b=$attrValue1 c='2' d=$attrValue2 e="3"/>"""
    val expected = Elem(QName("root"), Map(QName("a") -> "0", QName("b") -> "1", QName("c") -> "2", QName("d") -> "v", QName("e") -> "3"), Seq.empty)

    parsed should be(expected)
  }

  it should "drop the attribute if provided value is null" in {
    val value = null
    val parsed = xml"<root a=$value/>"
    val expected = Elem(QName("root"), Map.empty, Seq.empty)

    parsed should be(expected)
  }

  "an XML document expecting nodes" should "be correctly parsed a sequence of nodes is provided" in {
    val comment = Comment("a comment")
    val elem = Elem(QName("sub"), Map.empty, Seq.empty)
    val text = Text("text")
    val nodes = Seq(comment, elem, text)
    val parsed = xml"""<root><!-- let’s try to include nodes --><![CDATA[Some data]]>$nodes<sub>Another sub</sub></root>"""
    val expected =
      Elem(QName("root"), Map.empty, Seq(
        CDATA("Some data"),
        comment,
        elem,
        text,
        Elem(QName("sub"), Map.empty, Seq(
          Text("Another sub")))))

    parsed should be(expected)
  }

}
