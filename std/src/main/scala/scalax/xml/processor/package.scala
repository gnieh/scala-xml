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

import tree._

package object processor {
  self =>

  type XmlTransformer[State] = Transformer[State, XmlNode, XmlNode]

  def foldTopdown[In, Out](f: (Out, In) => Out): Folder[In, Out] =
    new TopDowFolder[In, Out] {
      def doNode(acc: Out, n: In): Out =
        f(acc, n)
    }

  def foldBottomup[In, Out](f: (Out, In) => Out): Folder[In, Out] =
    new BottomUpFolder[In, Out] {
      def doNode(acc: Out, n: In): Out =
        f(acc, n)
    }

  def transform[State, In, Out](upd: (State, Tree[In]) => State, trans: (State, Tree[In]) => Out): Transformer[State, In, Out] = new Transformer[State, In, Out] {
    def updateState(previous: State, n: Tree[In]) = upd(previous, n)
    def transformNode(state: State, n: Tree[In]) = trans(state, n)
  }

  def map[In, Out](f: Tree[In] => Out): Mapper[In, Out] = new Mapper[In, Out] {
    def map(n: Tree[In]) = f(n)
  }

  implicit class TreeOps[Elt](val tree: Tree[Elt]) extends AnyVal {

    @inline
    def map[Elt1](f: Tree[Elt] => Elt1): Tree[Elt1] =
      self.map(f).transform(tree)

  }

}
