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

import ast._
import symtab._
import oz._
import transform._

/** Provides a method for building a program out of its parts */
object ProgramBuilder extends TreeDSL with TransformUtils {
  val treeCopy = new TreeCopier

  /** Builds a program that defines a regular functor
   *
   *  Given a functor expression <functor>, the generated procedure is
   *  straightforward:
   *  {{{
   *  proc {$ <Base> ?<Result>}
   *     <Result> = <functor>
   *  end
   *  }}}
   */
  def buildModuleProgram(prog: Program, functor: Expression) {
    prog.rawCode = {
      prog.topLevelResultSymbol === functor
    }
  }

  /** Builds a program that creates the base environment
   *
   *  The base functor, that we call <BaseFunctor> from here, must contain
   *  exactly one top-level functor definition, and should have the following
   *  structure:
   *  {{{
   *  functor
   *
   *  require
   *     Boot_ModA at 'x-oz://boot/ModA'
   *     ...
   *
   *  prepare
   *     ...
   *
   *  exports
   *     'SomeVar':SomeVar
   *     ...
   *  end
   *  }}}
   *
   *  The program statement applies this functor, giving it as imports all
   *  the boot modules. These are looked up in the boot modules map.
   *  The result of this application is returned as the top-level result.
   *
   *  Hence the program looks like this:
   *  {{{
   *  proc {$ ?<Result>}
   *     Imports = 'import'(
   *        'Boot_ModA': <constant looked up in the boot modules map>
   *        ...
   *        'Boot_ModN': <...>
   *     )
   *  in
   *     <Result> = {<BaseFunctor>.apply Imports}
   *  end
   *  }}}
   */
  def buildBaseEnvProgram(prog: Program,
      bootModulesMap: Map[String, Expression],
      baseFunctor0: Expression) {

    val baseFunctor = baseFunctor0.asInstanceOf[FunctorExpression]

    // Extract exports to fill in `prog.baseDeclarations`
    for (FunctorExport(Constant(OzAtom(name)), _) <- baseFunctor.exports) {
      prog.baseDeclarations += name
    }

    // Synthesize the program statement
    val wholeProgram = {
      val imports = {
        val reqs = baseFunctor.require ++ baseFunctor.imports

        val fields =
          for (FunctorImport(RawVariable(name), _, Some(location)) <- reqs)
            yield RecordField(OzAtom(name), bootModulesMap(location))

        Record(OzAtom("import"), fields.toList)
      }

      (baseFunctor dot OzAtom("apply")) call (
          imports, prog.topLevelResultSymbol)
    }

    prog.rawCode = wholeProgram
  }
}
