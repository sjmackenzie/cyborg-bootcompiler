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

import scala.collection.mutable.{ ArrayBuffer, Map }
import scala.util.parsing.input.{ Position, NoPosition }

import ast._
import bytecode._

/** Companion object of [[org.mozartoz.bootcompiler.symtab.Abstraction]] */
object Abstraction {
  private val nextID = (new util.Counter).next _
}

/** Abstraction */
class Abstraction(val owner: Abstraction, val name: String, val pos: Position) {
  /** Numeric ID of the abstraction */
  val id = Abstraction.nextID()

  /** Formal parameters */
  val formals = new ArrayBuffer[Symbol]

  /** Local variables */
  val locals = new ArrayBuffer[Symbol]

  /** Global variables, aka contextual environment */
  val globals = new ArrayBuffer[Symbol]

  /** Flags */
  val flags = new ArrayBuffer[String]

  /** AST of the body */
  var body: Statement = SkipStatement()

  /** Map from free variables to the corresponding global variable */
  private val _freeVarToGlobal = Map[Symbol, Symbol]()

  /** Code area */
  val codeArea = new CodeArea(this)

  /** Arity, i.e., number of formal parameters */
  def arity = formals.size

  /** Acquires a symbol as being declared in this abstraction */
  def acquire(symbol: Symbol) {
    symbol.setOwner(this)
    if (symbol.isFormal) formals += symbol
    else if (symbol.isGlobal) globals += symbol
    else locals += symbol
  }

  /** Full name of the abstraction, for display purposes */
  def fullName: String =
    if (owner == NoAbstraction) name
    else owner.fullName + "::" + name

  /** Creates a new abstraction that is inner to this abstraction */
  def newAbstraction(name: String, pos: Position) =
    new Abstraction(this, name, pos)

  /** Maps a free variable to the corresponding global variable
   *
   *  If no such global variable exists yet, it is created.
   */
  def freeVarToGlobal(symbol: Symbol) = {
    require(symbol.owner ne this)
    _freeVarToGlobal.getOrElseUpdate(symbol, {
      val global = symbol.copyAsGlobal()
      acquire(global)
      global
    })
  }

  /** Dumps the abstraction on standard error
   *
   *  @param includeByteCode include the bytecode in the dump
   */
  def dump(includeByteCode: Boolean = true) {
    println(fullName + ": P/" + arity.toString())
    println("  formals: " + (formals mkString " "))
    println("  locals: " + (locals mkString " "))
    println("  globals: " + (globals mkString " "))

    println()
    println(body)

    if (codeArea.isDefined) {
      println()
      codeArea.dump(includeByteCode)
    }
  }
}

/** No abstraction marker */
object NoAbstraction extends Abstraction(null, "<NoAbstraction>", NoPosition) {
  override val owner = this
}
