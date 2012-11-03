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

import scala.collection.mutable._

import oz._
import ast._
import symtab._

object Flattener extends Transformer with TreeDSL {
  private var globalToFreeVar: Map[Symbol, Symbol] = _

  override def apply() {
    val rawCode = program.rawCode
    program.rawCode = null

    withAbstraction(program.topLevelAbstraction) {
      program.topLevelAbstraction.body = transformStat(rawCode)
    }
  }

  private def withAbstraction[A](newAbs: Abstraction)(f: => A) = {
    val savedAbs = abstraction
    val savedGlobalToFreeVar = globalToFreeVar

    try {
      abstraction = newAbs
      globalToFreeVar = Map.empty
      f
    } finally {
      abstraction = savedAbs
      globalToFreeVar = savedGlobalToFreeVar
    }
  }

  override def transformStat(statement: Statement) = statement match {
    case LocalStatement(declarations, stat) =>
      for (variable <- declarations)
        abstraction.acquire(variable.symbol)

      transformStat(stat)

    case _ =>
      super.transformStat(statement)
  }

  override def transformExpr(expression: Expression) = expression match {
    case LocalExpression(declarations, expr) =>
      for (variable <- declarations)
        abstraction.acquire(variable.symbol)

      transformExpr(expr)

    case proc @ ProcExpression(name, args, body, flags) =>
      val abs = abstraction.newAbstraction(name, proc.pos)

      program.abstractions += abs

      for (Variable(symbol) <- args)
        abs.acquire(symbol)

      abs.flags ++= flags

      val (newBody, globalArgs) = withAbstraction(abs) {
        val newBody = transformStat(body)

        val globalArgs =
          for (global <- abs.globals.toList)
            yield Variable(globalToFreeVar(global))

        (newBody, globalArgs)
      }

      abs.body = newBody

      val newGlobalArgs = globalArgs map {
        g => transformExpr(g).asInstanceOf[Variable]
      }

      treeCopy.CreateAbstraction(proc,
          OzCodeArea(abs.codeArea), newGlobalArgs)

    case v @ FreeVar(sym) =>
      val global = abstraction.freeVarToGlobal(sym)
      globalToFreeVar += global -> sym
      treeCopy.Variable(v, global)

    case _ =>
      super.transformExpr(expression)
  }

  object FreeVar {
    def unapply(v: Variable) = {
      if (v.symbol.owner eq abstraction) None
      else Some(v.symbol)
    }
  }
}
