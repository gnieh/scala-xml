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
package tree

final case class Zipper[A](tree: Tree[A], context: BreadCrumbs[A]) {

  def up: Option[Zipper[A]] = context match {
    case Seq()             => None
    case Seq(bc, bcs @ _*) => Some(Zipper(up1(tree, bc), bcs))
  }

  def down: Option[Zipper[A]] = tree.children match {
    case Seq()           => None
    case Seq(c, cs @ _*) => Some(Zipper(c, Crumb(Nil, tree, cs) +: context))
  }

  def right: Option[Zipper[A]] = context match {
    case Seq()                                        => None
    case Seq(Crumb(_, _, Seq()), _*)                  => None
    case Seq(Crumb(ls, n, Seq(r, rs @ _*)), bcs @ _*) => Some(Zipper(r, Crumb(tree +: ls, n, rs) +: bcs))

  }

  def left: Option[Zipper[A]] = context match {
    case Seq()                                        => None
    case Seq(Crumb(Seq(), _, _), _*)                  => None
    case Seq(Crumb(Seq(l, ls @ _*), n, rs), bcs @ _*) => Some(Zipper(l, Crumb(ls, n, tree +: rs) +: bcs))
  }

  def addLeftSibling(t: Tree[A]): Option[Zipper[A]] = context match {
    case Seq()                           => None
    case Seq(Crumb(ls, n, rs), bcs @ _*) => Some(Zipper(tree, Crumb(t +: ls, n, rs) +: bcs))
  }

  def addRightSibling(t: Tree[A]): Option[Zipper[A]] = context match {
    case Seq()                           => None
    case Seq(Crumb(ls, n, rs), bcs @ _*) => Some(Zipper(tree, Crumb(ls, n, t +: rs) +: bcs))
  }

  def dropLeftSibling: Option[Zipper[A]] = context match {
    case Seq()                                        => None
    case Seq(Crumb(Seq(_, ls @ _*), n, rs), bcs @ _*) => Some(Zipper(tree, Crumb(ls, n, rs) +: bcs))
  }

  def dropRightSibling: Option[Zipper[A]] = context match {
    case Seq()                                        => None
    case Seq(Crumb(ls, n, Seq(_, rs @ _*)), bcs @ _*) => Some(Zipper(tree, Crumb(ls, n, rs) +: bcs))
  }

  private def up1(tree: Tree[A], crumb: Crumb[A]): Tree[A] =
    crumb match {
      case Crumb(ls, n, rs) =>
        n.withChildren(ls.foldLeft(tree +: rs)((acc, x) => x +: acc))
    }

}

object Zipper {

  def apply[A](n: Tree[A]): Zipper[A] =
    Zipper(n, Nil)

}
