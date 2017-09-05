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

abstract class TopDowFolder[In, Out] extends Folder[In, Out] {

  override final def process(init: Out, t: Tree[In]): Out = {
    @tailrec
    def loop(acc: Out, stack: Seq[Tree[In]]): Out =
      stack match {
        case Seq() =>
          acc
        case Seq(Tree(n, children), rest @ _*) =>
          val acc1 = doNode(acc, n)
          val stack1 = children ++ rest
          loop(acc1, stack1)
      }
    loop(init, Seq(t))
  }

}
