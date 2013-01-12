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
   *  Given a functor expression <functor>, the whole program is
   *  straightforward:
   *  {{{
   *  local
   *     <Base>
   *  in
   *     <Base> = {Boot_Property.get 'internal.boot.base' $ true}
   *     <Result> = <functor>
   *  end
   *  }}}
   */
  def buildModuleProgram(prog: Program, functor: Expression) {
    prog.rawCode = {
      LOCAL (prog.baseEnvSymbol) IN {
        (prog.baseEnvSymbol === getBootProperty(prog, "internal.boot.base")) ~
        (prog.topLevelResultSymbol === functor)
      }
    }
  }

  /** Builds a program that creates the base environment
   *
   *  The base functors must be functors indeed, and should have the following
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
   *  All the base functors are merged together as a single functor, that we
   *  call the base functor and write <BaseFunctor> from here.
   *
   *  The <BaseFunctor> must export an unbound variable under feature 'Base'.
   *
   *  The program statement applies this functor, giving it as imports all
   *  the boot modules. These are looked up in the boot modules map.
   *  The result of this application is returned as the top-level result.
   *
   *  Hence the program looks like this:
   *  {{{
   *  local
   *     <Base>
   *     Imports = 'import'(
   *        'Boot_ModA': <constant looked up in the boot modules map>
   *        ...
   *        'Boot_ModN': <...>
   *     )
   *  in
   *     <Base> = {<BaseFunctor>.apply Imports}
   *     <Result> = <Base>
   *  end
   *  }}}
   */
  def buildBaseEnvProgram(prog: Program,
      bootModulesMap: Map[String, Expression],
      baseFunctors: List[Expression]) {

    // Merge all the base functors in one
    val baseFunctor = mergeBaseFunctors(
        baseFunctors map (_.asInstanceOf[FunctorExpression]))

    // Extract exports to fill in `prog.baseDeclarations`
    for (FunctorExport(Constant(OzAtom(name)), _) <- baseFunctor.exports) {
      prog.baseDeclarations += name
    }

    // Now starts the synthesis of the program statement

    // Application of the base functor
    val applyBaseFunctorStat = {
      val imports = {
        val reqs = baseFunctor.require ++ baseFunctor.imports

        val fields =
          for (FunctorImport(RawVariable(name), _, Some(location)) <- reqs)
            yield RecordField(OzAtom(name), bootModulesMap(location))

        Record(OzAtom("import"), fields.toList)
      }

      (baseFunctor dot OzAtom("apply")) call (imports, prog.baseEnvSymbol)
    }

    // Put things together
    val wholeProgram = {
      LOCAL (prog.baseEnvSymbol) IN {
        applyBaseFunctorStat ~
        (prog.topLevelResultSymbol === prog.baseEnvSymbol)
      }
    }

    prog.rawCode = wholeProgram
  }

  private def mergeBaseFunctors(functors: List[FunctorExpression]) = {
    atPos(functors.head) {
      functors.tail.foldLeft(functors.head) { (lhs, rhs) =>
        val FunctorExpression(lhsName, lhsRequire, lhsPrepare,
            lhsImports, lhsDefine, lhsExports) = lhs

        val FunctorExpression(rhsName, rhsRequire, rhsPrepare,
            rhsImports, rhsDefine, rhsExports) = rhs

        FunctorExpression(if (lhsName.isEmpty) rhsName else lhsName,
            lhsRequire ::: rhsRequire, mergePrepares(lhsPrepare, rhsPrepare),
            lhsImports ::: rhsImports, mergePrepares(lhsDefine, rhsDefine),
            lhsExports ::: rhsExports)
      }
    }
  }

  private def mergePrepares(lhs: Option[LocalStatementOrRaw],
      rhs: Option[LocalStatementOrRaw]) = {
    if (lhs.isEmpty) rhs
    else if (rhs.isEmpty) lhs
    else {
      val RawLocalStatement(lhsDecls, lhsStat) = lhs.get
      val RawLocalStatement(rhsDecls, rhsStat) = rhs.get

      Some(RawLocalStatement(lhsDecls ::: rhsDecls, lhsStat ~ rhsStat))
    }
  }

  private def getBootProperty(prog: Program, property: String) = {
    prog.builtins.getProperty callExpr (
        OzAtom(property), NestingMarker(), True())
  }
}
