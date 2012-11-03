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
package transform

import ast._

trait TransformUtils {
  def statementsToStatement(statements: List[Statement]) = statements match {
    case Nil => SkipStatement()
    case stat :: Nil => stat
    case _ => CompoundStatement(statements)
  }

  def statsAndStatToStat(statements: List[Statement], statement: Statement) =
    if (statements.isEmpty) statement
    else CompoundStatement(statements :+ statement)

  def statsAndExprToExpr(statements: List[Statement], expression: Expression) =
    if (statements.isEmpty) expression
    else StatAndExpression(statementsToStatement(statements), expression)
}
