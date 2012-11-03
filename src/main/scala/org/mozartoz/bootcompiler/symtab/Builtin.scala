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
package symtab

/** Companion object for Builtin */
object Builtin {
  /** Parameter kind */
  object ParamKind extends Enumeration {
    val In, Out = Value
  }

  /** Parameter kind */
  type ParamKind = ParamKind.Value
}

/** Builtin procedure of the host VM */
class Builtin(val moduleName: String, val name: String,
    val ccFullGetter: String,
    val paramKinds: List[Builtin.ParamKind],
    val inlineAs: Option[Int]) {

  override def toString() =
    moduleName + "." + (if (name.charAt(0).isLetter) name else "'" + name + "'")

  val arity = paramKinds.size

  val inlineable = inlineAs.isDefined
  def inlineOpCode = inlineAs.get
}
