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

import symtab._

trait RawDeclarationOrVar extends Node
trait RawDeclaration extends RawDeclarationOrVar

trait InfixSyntax extends Node {
  protected val left: Node
  protected val right: Node
  protected val opSyntax: String

  def syntax(indent: String) = {
    val untilOp = left.syntax(indent) + opSyntax
    val rightIndent = indent + " "*untilOp.length
    untilOp + right.syntax(rightIndent)
  }
}

trait MultiInfixSyntax extends Node {
  protected val operands: Seq[Node]
  protected val operators: Seq[String]

  def syntax(indent: String) = {
    val first = operands.head.syntax(indent)
    (operators zip operands.tail).foldLeft(first) {
      case (prev, (op, operand)) =>
        val untilOp = prev + op
        untilOp + operand.syntax(indent + " "*untilOp.length)
    }
  }
}
