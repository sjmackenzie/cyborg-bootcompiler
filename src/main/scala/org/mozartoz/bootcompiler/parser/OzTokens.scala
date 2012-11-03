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
package parser

import scala.util.parsing.combinator.token._

trait OzTokens extends StdTokens {
  /** The class of integer literal tokens */
  case class IntLit(value: Long) extends Token {
    override def chars = value.toString
    override def toString() = chars
  }

  /** The class of float literal tokens */
  case class FloatLit(value: Double) extends Token {
    override def chars = value.toString
    override def toString() = chars
  }

  /** The class of atom literal tokens */
  case class AtomLit(chars: String) extends Token {
    override def toString() = "atom "+chars
  }

  /** The class of char literal tokens */
  case class CharLit(char: Char) extends Token {
    override def chars = "&" + char
    override def toString() = chars
  }

  /** A special token representing a label, i.e. an identifier or atom literal
   *  followed directly by a (
   */
  case class Label(label: Token) extends Token {
    override def chars = label.chars + "("
    override def toString() = label.toString() + "("
  }

  /** Preprocessor switch */
  case class PreprocessorSwitch(switch: String, value: Boolean) extends Token {
    override def chars = "\\switch " + (if (value) "+" else "-") + switch
    override def toString() = chars
  }

  /** Preprocessor directive */
  case class PreprocessorDirective(directive: String) extends Token {
    override def chars = "\\" + directive
    override def toString() = chars
  }

  /** Preprocessor directive with argument */
  case class PreprocessorDirectiveWithArg(directive: String,
      arg: String) extends Token {
    override def chars = "\\" + directive + " " + arg
    override def toString() = chars
  }
}
