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

import oz._
import ast._
import symtab._

object Desugar extends Transformer with TreeDSL {
  override def transformStat(statement: Statement) = statement match {
    case assign @ BinaryOpStatement(lhs, ":=", rhs) =>
      builtins.catAssign call (transformExpr(lhs), transformExpr(rhs))

    case DotAssignStatement(left, center, right) =>
      builtins.dotAssign call (transformExpr(left), transformExpr(center),
          transformExpr(right))

    case ifStat @ IfStatement(cond, trueStat, falseStat:NoElseStatement) =>
      transformStat {
        treeCopy.IfStatement(ifStat, cond, trueStat,
            treeCopy.SkipStatement(falseStat))
      }

    case thread @ ThreadStatement(body) =>
      atPos(thread) {
        val proc = PROC("", Nil) {
          transformStat(body)
        }

        builtins.createThread call (proc)
      }

    case lockStat @ LockStatement(lock, body) =>
      atPos(lockStat) {
        val proc = PROC("", Nil) {
          transformStat(body)
        }

        baseEnvironment("LockIn") call (transformExpr(lock), proc)
      }

    case TryFinallyStatement(body, finallyBody) =>
      transformStat {
        atPos(statement) {
          statementWithTemp { tempX =>
            val tempY = Variable.newSynthetic(capture = true)

            (LOCAL (tempY) IN {
              (tempX === TryExpression(body ~> UnitVal(),
                  tempY, Tuple(OzAtom("ex"), List(tempY))))
            }) ~
            finallyBody ~
            (IF (tempX =?= UnitVal()) THEN {
              SkipStatement()
            } ELSE {
              RaiseStatement(tempX dot OzInt(1))
            })
          }
        }
      }

    case _ =>
      super.transformStat(statement)
  }

  override def transformExpr(expression: Expression) = expression match {
    case fun @ FunExpression(name, args, body, flags) =>
      val result = Variable.newSynthetic("<Result>", formal = true)

      val isLazy = flags contains "lazy"
      val newFlags =
        if (isLazy) flags filterNot "lazy".==
        else flags

      val proc = atPos(fun) {
        PROC(name, args :+ result, newFlags) {
          if (isLazy) {
            THREAD {
              (builtins.waitNeeded call (result)) ~
              (result === body)
            }
          } else {
            result === body
          }
        }
      }

      transformExpr(proc)

    case thread @ ThreadExpression(body) =>
      expressionWithTemp { temp =>
        transformStat(atPos(thread) {
          THREAD (temp === body)
        }) ~> temp
      }

    case lockExpr @ LockExpression(lock, body) =>
      expressionWithTemp { temp =>
        transformStat(atPos(lockExpr) {
          LockStatement(lock, temp === body)
        }) ~> temp
      }

    case TryFinallyExpression(body, finallyBody) =>
      transformExpr {
        atPos(expression) {
          expressionWithTemp { tempX =>
            val tempY = Variable.newSynthetic(capture = true)

            (LOCAL (tempY) IN {
              (tempX === TryExpression(
                  Tuple(OzAtom("ok"), List(body)),
                  tempY, Tuple(OzAtom("ex"), List(tempY))))
            }) ~
            finallyBody ~>
            (IF ((builtins.label callExpr (tempX)) =?= OzAtom("ok")) THEN {
              tempX dot OzInt(1)
            } ELSE {
              RaiseExpression(tempX dot OzInt(1))
            })
          }
        }
      }

    case DotAssignExpression(left, center, right) =>
      transformExpr(builtins.dotExchange callExpr (left, center, right))

    case BinaryOp(lhs, "+", Constant(OzInt(1))) =>
      transformExpr(builtins.plus1 callExpr (lhs))

    case BinaryOp(lhs, "-", Constant(OzInt(1))) =>
      transformExpr(builtins.minus1 callExpr (lhs))

    case UnaryOp(op, arg) =>
      transformExpr(builtins.unaryOpToBuiltin(op) callExpr (arg))

    case BinaryOp(lhs, op, rhs) =>
      transformExpr(builtins.binaryOpToBuiltin(op) callExpr (lhs, rhs))

    case ShortCircuitBinaryOp(lhs, "andthen", rhs) =>
      transformExpr(IF (lhs) THEN (rhs) ELSE (False()))

    case ShortCircuitBinaryOp(lhs, "orelse", rhs) =>
      transformExpr(IF (lhs) THEN (True()) ELSE (rhs))

    case Record(label, fields) =>
      val fieldsNoAuto = fillAutoFeatures(fields)
      val newRecord = treeCopy.Record(expression, label, fieldsNoAuto)
      super.transformExpr(newRecord)

    case OpenRecordPattern(label, fields) =>
      val fieldsNoAuto = fillAutoFeatures(fields)
      val newPattern = treeCopy.OpenRecordPattern(
          expression, label, fieldsNoAuto)
      super.transformExpr(newPattern)

    case _ =>
      super.transformExpr(expression)
  }

  private def fillAutoFeatures(fields: List[RecordField]) = {
    if (fields forall (!_.hasAutoFeature)) {
      // Trivial case: all features are non-auto
      fields
    } else if (fields forall (_.hasAutoFeature)) {
      // Next-to-trivial case: all features are auto
      for ((field, index) <- fields.zipWithIndex)
        yield treeCopy.RecordField(field, OzInt(index+1), field.value)
    } else {
      // Complex case: mix of auto and non-auto features

      // Collect used integer features
      val usedFeatures = (for {
        RecordField(Constant(OzInt(feature)), _) <- fields
      } yield feature).toSet

      // Actual filling
      var nextFeature: Long = 1

      for (field @ RecordField(feature, value) <- fields) yield {
        if (field.hasAutoFeature) {
          while (usedFeatures contains nextFeature)
            nextFeature += 1
          nextFeature += 1

          val newFeature = treeCopy.Constant(feature, OzInt(nextFeature-1))
          treeCopy.RecordField(field, newFeature, value)
        } else {
          field
        }
      }
    }
  }
}
