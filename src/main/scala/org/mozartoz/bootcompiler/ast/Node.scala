// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.


package org.mozartoz.bootcompiler
package ast

import scala.util.parsing.input.Positional

/** Node of an Oz AST
 *
 *  There are two important subclasses of `Node`:
 *  [[org.mozartz.bootcompiler.ast.Statement]] and
 *  [[org.mozartz.bootcompiler.ast.Expression]], with obvious meanings.
 */
abstract class Node extends Product with Positional {

  /** Copy the attributes of a node into this `Node`. */
  private[bootcompiler] def copyAttrs(tree: Node): this.type = {
    pos = tree.pos
    this
  }

  /** Returns a pretty-printed representation of this `Node`
   *
   *  @param indent indentation to use when writing a line feed
   */
  def syntax(indent: String = ""): String

  override def toString = syntax()

  /** Pre-order walk of the subtree rooted at this `Node`
   *
   *  At each node, the `handler` is called. If it returns `true`, then the
   *  walk dives into the children of this `Node`. Otherwise, it does not.
   *
   *  @param handler handler callback
   */
  def walkBreak(handler: Node => Boolean) {
    if (handler(this)) {
      def inner(element: Any) {
        element match {
          case node:Node => node.walk(handler)
          case seq:Seq[_] => seq foreach inner
          case _ => ()
        }
      }

      productIterator foreach inner
    }
  }

  /** Pre-order walk of the subtree rooted at this `Node`
   *
   *  At each node, the `handler` is called.
   *
   *  @param handler handler callback
   */
  def walk[U](handler: Node => U) {
    walkBreak { node =>
      handler(node)
      true
    }
  }
}
