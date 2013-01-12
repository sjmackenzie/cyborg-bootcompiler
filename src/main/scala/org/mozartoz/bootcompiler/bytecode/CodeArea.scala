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
package bytecode

import scala.collection.mutable._

import oz._
import ast._
import symtab._
import util._

class CodeArea(val abstraction: Abstraction) {
  val opCodes = new ArrayBuffer[OpCode]

  private val symbolRegisterAllocs = new HashMap[Symbol, YOrGReg]
  private val constantRegisterAllocs = new HashMap[OzValue, KReg]

  private val YCounter = new RegCounter(YReg)

  val constants = new ArrayBuffer[OzValue]

  def isDefined = !opCodes.isEmpty

  def size = opCodes.map(_.size).sum

  override def toString() = "<CodeArea %s>" format (abstraction.fullName)

  def dump(includeByteCode: Boolean = true) {
    println("constants:")
    for ((constant, index) <- constants.zipWithIndex)
      println("  K(%d) = %s" format (index, constant))

    if (includeByteCode) {
      println()
      for (opCode <- opCodes)
        println(opCode.code)
    }
  }

  def registerFor(symbol: Symbol): YOrGReg = {
    symbolRegisterAllocs.getOrElseUpdate(symbol, {
      if (symbol.isGlobal) GReg(symbol.owner.globals.indexOf(symbol))
      else YCounter.next()
    })
  }

  def registerFor(value: OzValue): KReg = {
    constantRegisterAllocs.getOrElseUpdate(value, {
      constants += value
      KReg(constants.size - 1)
    })
  }

  def registerFor(expr: VarOrConst): Register = expr match {
    case variable:Variable =>
      registerFor(variable.symbol)

    case constant:Constant =>
      registerFor(constant.value)
  }

  def += (opCode: OpCode) =
    opCodes += opCode

  class Hole(index: Int) {
    def fillWith(opCode: OpCode) {
      opCodes(index) = opCode
    }
  }

  def addHole(size: Int = 1) = {
    val index = opCodes.size
    opCodes += OpHole(size)
    new Hole(index)
  }

  def counting(body: => Unit) = {
    val before = opCodes.size
    body
    (before until opCodes.size) map (i => opCodes(i).size) sum
  }

  def computeXCount() = {
    var maxX = 0

    for {
      opCode <- opCodes
      XReg(index) <- opCode.arguments
    } {
      maxX = maxX max index
    }

    maxX + 1
  }

  lazy val debugData = {
    implicit def pair2recordField(pair: (OzFeature, OzValue)) =
      OzRecordField(pair._1, pair._2)

    val pos = abstraction.pos
    val fileName = FilePosition.fileNameOf(pos, "")

    OzRecord(OzAtom("d"), List(
        OzAtom("column") -> OzInt(pos.column),
        OzAtom("file") -> OzAtom(fileName),
        OzAtom("line") -> OzInt(pos.line)
    ))
  }
}
