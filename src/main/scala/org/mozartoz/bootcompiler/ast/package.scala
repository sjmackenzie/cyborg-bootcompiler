package org.mozartoz.bootcompiler

import scala.util.parsing.input.{ Position, NoPosition, Positional }

/** Classes representing the AST of Oz code
 *
 *  Provides general utilities for working with ASTs.
 */
package object ast {
  // Utils

  def escapePseudoChars(name: String, delim: Char) = {
    val result = new StringBuffer
    name foreach { c =>
      if (c == '\\' || c == delim)
        result append '\\'
      result append c
    }
    result.toString
  }

  /** Implicit conversion from a pair of expressions to a record field */
  implicit def pair2recordField(pair: (Expression, Expression)) =
    RecordField(pair._1, pair._2)

  /** Implicit conversion from an expression to record/tuple field */
  implicit def expr2recordField(expr: Expression) =
    RecordField(AutoFeature(), expr)

  /** Gives a position to a subtree
   *
   *  The position `pos` is given to `node` and all its subtrees that do not
   *  yet have a position. If a subtree has a position, its children are not
   *  explored.
   *
   *  This method is mostly useful for synthesized AST subtrees, as in
   *  {{{
   *  val synthesized = atPos(oldTree.pos) {
   *    buildTheTree()
   *  }
   *  }}}
   *
   *  @tparam A type of node
   *  @param pos position to give to the subtree
   *  @param node root of the subtree to give a position to
   *  @return the node `node`
   */
  def atPos[A <: Node](pos: Position)(node: A): A = {
    node walkBreak { subNode =>
      if (subNode.pos ne NoPosition) false
      else {
        subNode.setPos(pos)
        true
      }
    }

    node
  }

  /** Gives a position to a subtree
   *
   *  This is similar to the other overload of `atPos()`, except that it takes
   *  a [[scala.util.parsing.input.Positional]]. The position is extracted from
   *  the given positional.
   */
  def atPos[A <: Node](positional: Positional)(node: A): A =
    atPos(positional.pos)(node)
}
