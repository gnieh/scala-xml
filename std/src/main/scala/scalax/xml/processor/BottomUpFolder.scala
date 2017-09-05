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
package processor

import tree._

import scala.annotation.tailrec

abstract class BottomUpFolder[In, Out] extends Folder[In, Out] {

  override final def process(init: Out, t: Tree[In]): Out = {
    @tailrec
    def down(n: Tree[In], stack: Seq[Tree[In]]): Seq[Tree[In]] =
      n match {
        case Tree(n, Seq(fst, rest @ _*)) =>
          down(fst, Tree(n, rest) +: stack)
        case Tree(_, Seq()) =>
          n +: stack
      }
    @tailrec
    def up(acc: Out, stack: Seq[Tree[In]]): Out =
      stack match {
        case Seq() =>
          acc
        case Seq(Tree(n, Seq()), rest @ _*) =>
          val acc1 = doNode(acc, n)
          up(acc1, rest)
        case Seq(fst, rest @ _*) =>
          val stack1 = down(fst, rest)
          up(acc, stack1)
      }
    up(init, down(t, Seq()))
  }

}

