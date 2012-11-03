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

sealed abstract class OpCodeArg {
  def encoding: Int

  def code = encoding.toString()
}

case class ImmInt(value: Int) extends OpCodeArg {
  override def encoding = value

  override def toString() = value.toString()
}

sealed abstract class Register(letter: String) extends OpCodeArg {
  val index: Int

  override def encoding = index

  override def toString() = "%s(%d)" format (letter, index)
}

sealed trait NotKReg extends Register
sealed trait XOrYReg extends NotKReg
sealed trait XOrGReg extends NotKReg
sealed trait XOrKReg extends Register
sealed trait YOrGReg extends NotKReg

case class XReg(index: Int) extends Register("X")
    with XOrYReg with XOrGReg with XOrKReg

case class YReg(index: Int) extends Register("Y") with XOrYReg with YOrGReg
case class GReg(index: Int) extends Register("G") with XOrGReg with YOrGReg
case class KReg(index: Int) extends Register("K") with XOrKReg
