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

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.token._

import collection.mutable.HashMap

trait OzTokenParsers extends StdTokenParsers with OzPreprocessor {
  type Tokens = OzTokens
  //val lexical = new OzLexical

  import lexical._

  /** A parser which matches an int literal */
  def intLit: Parser[Long] =
    accept("int literal", { case IntLit(value) => value })

  /** A parser which matches a float literal */
  def floatLit: Parser[Double] =
    accept("float literal", { case FloatLit(value) => value })

  /** A parser which matches an atom literal */
  def atomLit: Parser[String] =
    elem("atom literal", _.isInstanceOf[AtomLit]) ^^ (_.chars)

  /** A parser which matches an atom literal */
  def charLit: Parser[Char] =
    accept("char literal", { case CharLit(char) => char })

  /** A parser which matches an atom literal label */
  def atomLitLabel: Parser[String] =
    acceptMatch("atom literal label", {
      case Label(AtomLit(chars)) => chars
    })

  /** A parser which matches an identifier label */
  def identLabel: Parser[String] =
    acceptMatch("identifier label", {
      case Label(Identifier(chars)) => chars
    })

  /* an implicit keyword function that gives a warning when a given word is
   * not in the reserved/delimiters list
   */
  override implicit def keyword(chars : String): Parser[String] =
    if (lexical.reserved.contains(chars) || lexical.delimiters.contains(chars))
      super.keyword(chars)
    else {
      failure("You are trying to parse \""+chars+"\", but it is neither "+
          "contained in the delimiters list, nor in the reserved keyword list "+
          "of your lexical object")
    }
}
