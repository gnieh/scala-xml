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

/** Transforms a tree into another tree using a state computing from the root
 *  to the current node.
 *  `updateState` is called before `transformNode` so that current
 *  node takes the new state into account.
 *  Node is transformed before its children.
 */
abstract class Transformer[State, In, Out] {

  protected def updateState(previous: State, n: Tree[In]): State

  protected def transformNode(state: State, n: Tree[In]): Out

  def transform(state: State, t: Tree[In]): Tree[Out] = t match {
    case Tree(_, children) =>
      val state1 = updateState(state, t)
      val n1 = transformNode(state1, t)
      val children1 = children.map(transform(state1, _))
      Tree(n1, children1)
  }

}
