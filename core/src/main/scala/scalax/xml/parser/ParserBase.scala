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

import scala.io.Source

import scala.collection.immutable.Queue

import scala.annotation.{
  tailrec,
  switch
}

abstract class ParserBase(private var inputs: Queue[Source], protected var is11: Boolean) extends AutoCloseable {

  def this(input: Source, is11: Boolean) =
    this(Queue(input), is11)

  import XmlUtils._

  /** Feeds this parser with more content from the given source. */
  def feed(src: Source): Unit =
    inputs = inputs.enqueue(src)

  protected def fail(prod: String, msg: String): Nothing =
    fail(XmlSyntax(prod), msg)

  protected def fail(error: XmlError, msg: String): Nothing =
    throw XmlException(new LineColumnPosition(line, column), error, msg)

  private def currentInput = inputs.headOption

  protected def line = currentInput.fold(0)(_.RelaxedPositioner.cline)

  protected def column = currentInput.fold(0)(_.RelaxedPositioner.ccol)

  def close(): Unit =
    inputs.foreach(_.close())

  private var buffer = Option.empty[Char]

  @tailrec
  final protected def readInput(): Unit =
    inputs.dequeueOption match {
      case Some((input, rest)) =>
        if (input.hasNext) {
          val c = input.next()
          if (isValid(c))
            buffer = Some(c)
          else
            fail("2", "forbidden character")
        } else {
          input.close()
          inputs = rest
          readInput()
        }
      case None =>
        buffer = None
    }

  protected def peekChar(): Option[Char] =
    buffer.orElse {
      readInput()
      buffer
    }

  protected def nextCharOpt(): Option[Char] = {
    val c = peekChar()
    buffer = None
    c
  }

  /** Consumes the next character in source and returns it. */
  protected def nextChar(): Char =
    nextCharOpt().getOrElse(fail("1", "Unexpected end of input"))

  protected def accept(c: Char, error: String, msg: String): Unit = peekChar() match {
    case Some(n) if n == c =>
      nextChar()
    case _ =>
      fail(error, msg)
  }

  protected def assert(p: Char => Boolean, error: String, msg: String): Char = peekChar() match {
    case Some(c) if p(c) =>
      nextChar()
    case _ =>
      fail(error, msg)
  }

  protected def untilChar(p: Char => Boolean, sb: StringBuilder): StringBuilder = {
    @tailrec
    def loop(sb: StringBuilder): StringBuilder =
      peekChar() match {
        case Some(c) if !p(c) =>
          nextChar()
          loop(sb.append(c))
        case _ =>
          sb
      }
    loop(sb)
  }

  protected def read(s: String): Int = {
    @tailrec
    def loop(idx: Int, acc: Int): Int =
      if (idx >= s.length)
        acc
      else peekChar() match {
        case Some(c) if c == s.charAt(idx) =>
          nextChar()
          loop(idx + 1, acc + 1)
        case _ =>
          acc
      }
    loop(0, 0)
  }

  protected def isValid(c: Int): Boolean =
    if (is11)
      // [#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
      (0x1 <= c && c <= 0xd7ff) || (0xe000 <= c && c <= 0xfffd) || (0x10000 <= c && c <= 0x10ffff)
    else
      // #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
      c == 0x9 || c == 0xa || c == 0xd || (0x20 <= c && c <= 0xd7ff) || (0xe000 <= c && c <= 0xfffd) || (0x10000 <= c && c <= 0x10ffff)

  protected def space(): Boolean = {
    @tailrec
    def loop(acc: Boolean): Boolean =
      peekChar() match {
        case Some(c) if isXmlWhitespace(c) =>
          nextChar()
          loop(true)
        case _ =>
          acc
      }
    loop(false)
  }

  /** We have read '<!-' so far */
  protected def skipComment(l: Int, c: Int): CommentToken = {
    accept('-', "15", "second dash missing to open comment")
    def loop(): Unit = (nextChar(): @switch) match {
      case '-' =>
        (nextChar(): @switch) match {
          case '-' =>
            accept('>', "15", "'--' is not inside comments")
          case _ =>
            loop()
        }
      case _ =>
        loop()
    }
    loop()
    CommentToken(l, c)
  }

}
