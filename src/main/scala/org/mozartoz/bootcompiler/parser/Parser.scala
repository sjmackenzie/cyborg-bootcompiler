package org.mozartoz.bootcompiler
package parser

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.input._

import oz._
import ast._

/**
 * The main Oz parser
 */
class OzParser extends OzTokenParsers with PackratParsers
    with ImplicitConversions {

  lexical.reserved ++= List(
      "andthen", "at", "attr", "case", "catch", "choice",
      "class", "cond", "declare", "define", "dis",
      "div", "else", "elsecase", "elseif", "end",
      "export", "fail", "false", "feat", "finally", "from",
      "fun", "functor", "if", "import", "in", "local",
      "lock", "meth", "mod", "not", "of", "or", "orelse",
      "prepare", "proc", "prop", "raise", "require",
      "self", "skip", "then", "thread", "true", "try",
      "unit"
  )

  lexical.delimiters ++= List(
      "(", ")", "[", "]", "{", "}",
      "|", "#", ":", "...", "=", ".", ":=", "^", "[]", "$",
      "!", "_", "~", "+", "-", "*", "/", "@", "<-",
      ",", "!!", "<=", "==", "\\=", "<", "=<", ">",
      ">=", "=:", "\\=:", "<:", "=<:", ">:", ">=:", "::", ":::"
  )

  def parseStatement(input: Reader[Char]) =
    phrase(statement)(new lexical.Scanner(input))

  def parseExpression(input: Reader[Char]) =
    phrase(expression)(new lexical.Scanner(input))

  // Statements

  lazy val statement: PackratParser[Statement] =
    rep1(oneStatement) ^^ CompoundStatement

  lazy val oneStatement: PackratParser[Statement] = (
      bindStatement
    | "local" ~> inStatement <~ "end"
    | "(" ~> inStatement <~ ")"
    | procStatement
    | funStatement
    | callStatement
    | ifStatement
    | caseStatement
    | threadStatement
    | functorStatement
    | skipStatement
  )

  // Expressions

  lazy val expression: PackratParser[Expression] =
    expression0

  // Declarations

  lazy val inStatement: PackratParser[Statement] = (
      positioned((declarations <~ "in") ~ statement ^^ LocalStatement)
    | statement
  )

  lazy val inExpression: PackratParser[Expression] = (
      positioned((declarations <~ "in") ~ statExpression ^^ LocalExpression)
    | statExpression
  )

  lazy val statExpression: PackratParser[Expression] = positioned(
      expression
  ||| positioned(oneStatement ~ statExpression ^^ StatAndExpression)
  )

  lazy val declarations: PackratParser[List[Declaration]] =
    declaration+

  lazy val declaration: PackratParser[Declaration] = (
      oneStatement
    | variable
  )

  // Procedure and function definition

  lazy val procStatement: PackratParser[Statement] = positioned {
    (("proc" ~> procFlags <~ "{") ~ expression ~ formalArgs <~ "}") ~ inStatement <~ "end" ^^ {
      case flags ~ left ~ args ~ body =>
        val name = left match {
          case Variable(name) => name
          case _ => ""
        }
        BindStatement(left, ProcExpression(name, args, body, flags))
    }
  }

  lazy val funStatement: PackratParser[Statement] = positioned {
    (("fun" ~> procFlags <~ "{") ~ expression ~ formalArgs <~ "}") ~ inExpression <~ "end" ^^ {
      case flags ~ left ~ args ~ body =>
        val name = left match {
          case Variable(name) => name
          case _ => ""
        }
        BindStatement(left, FunExpression(name, args, body, flags))
    }
  }

  lazy val procExpression: PackratParser[Expression] = positioned {
    (("proc" ~> procFlags <~ "{" ~ "$") ~ formalArgs <~ "}") ~ inStatement <~ "end" ^^ {
      case flags ~ args ~ body => ProcExpression("", args, body, flags)
    }
  }

  lazy val funExpression: PackratParser[Expression] = positioned {
    (("fun" ~> procFlags <~ "{" ~ "$") ~ formalArgs <~ "}") ~ inExpression <~ "end" ^^ {
      case flags ~ args ~ body => FunExpression("", args, body, flags)
    }
  }

  lazy val procFlags = rep(atom ^^ (_.value))

  lazy val formalArgs = rep(formalArg)

  lazy val formalArg = variable

  // Call

  lazy val callStatement: PackratParser[Statement] = positioned {
    "{" ~> expression ~ actualArgs <~ "}" ^^ CallStatement
  }

  lazy val callExpression: PackratParser[Expression] = positioned {
    "{" ~> expression ~ actualArgs <~ "}" ^^ CallExpression
  }

  lazy val actualArgs = rep(expression)

  // If then else end

  lazy val ifStatement: PackratParser[Statement] = positioned {
    "if" ~> innerIfStatement <~ "end"
  }

  lazy val ifExpression: PackratParser[Expression] = positioned {
    "if" ~> innerIfExpression <~ "end"
  }

  lazy val innerIfStatement: PackratParser[Statement] = // !positioned
    expression ~ ("then" ~> inStatement) ~ elseStatement ^^ IfStatement

  lazy val innerIfExpression: PackratParser[Expression] = // !positioned
    expression ~ ("then" ~> inExpression) ~ elseExpression ^^ IfExpression

  // case of

  lazy val caseStatement: PackratParser[Statement] = positioned {
    "case" ~> innerCaseStatement <~ "end"
  }

  lazy val caseExpression: PackratParser[Expression] = positioned {
    "case" ~> innerCaseExpression <~ "end"
  }

  lazy val innerCaseStatement: PackratParser[Statement] = ( // !positioned
      expression ~ ("of" ~> caseStatementClauses) ~ elseStatement
          ^^ MatchStatement
  )

  lazy val innerCaseExpression: PackratParser[Expression] = ( // !positioned
      expression ~ ("of" ~> caseExpressionClauses) ~ elseExpressionCase
          ^^ MatchExpression
  )

  lazy val caseStatementClauses: PackratParser[List[MatchStatementClause]] =
    rep1(caseStatementClause, "[]" ~> caseStatementClause)

  lazy val caseStatementClause: PackratParser[MatchStatementClause] = (
      positioned(
          expression ~ opt("if" ~> expression) ~ ("then" ~> inStatement)
              ^^ MatchStatementClause)
  )

  lazy val caseExpressionClauses: PackratParser[List[MatchExpressionClause]] =
    rep1(caseExpressionClause, "[]" ~> caseExpressionClause)

  lazy val caseExpressionClause: PackratParser[MatchExpressionClause] = (
      positioned(
          expression ~ opt("if" ~> expression) ~ ("then" ~> inExpression)
              ^^ MatchExpressionClause)
  )

  // else clauses of if and case

  lazy val elseStatement: PackratParser[Statement] = (
      "else" ~> inStatement
    | positioned("elseif" ~> innerIfStatement)
    | positioned("elsecase" ~> innerCaseStatement)
    | positioned(success(SkipStatement()))
  )

  lazy val elseExpression: PackratParser[Expression] = (
      "else" ~> inExpression
    | positioned("elseif" ~> innerIfExpression)
    | positioned("elsecase" ~> innerCaseExpression)
  )

  lazy val elseExpressionCase: PackratParser[Expression] = (
      elseExpression
    | positioned(success(Constant(OzAtom("matchError"))))
  )

  // Thread

  lazy val threadStatement: PackratParser[Statement] = positioned {
    "thread" ~> inStatement <~ "end" ^^ ThreadStatement
  }

  lazy val threadExpression: PackratParser[Expression] = positioned {
    "thread" ~> inExpression <~ "end" ^^ ThreadExpression
  }

  // Bind and similar

  lazy val bindStatement: PackratParser[Statement] = positioned(
      (expression1 <~ "=") ~ expression0 ^^ BindStatement
    | (expression2 <~ ":=") ~ expression1 ^^ AssignStatement
  )

  // Functor

  lazy val functorStatement: PackratParser[Statement] = positioned {
    "functor" ~> expression ~ innerFunctor <~ "end" ^^ {
      case lhs ~ functor =>
        val name = lhs match {
          case Variable(name) => name
          case _ => ""
        }
        BindStatement(lhs, functor.copy(name = name))
    }
  }

  lazy val functorExpression: PackratParser[Expression] =
    "functor" ~> opt("$") ~> innerFunctor <~ "end"

  lazy val innerFunctor: PackratParser[FunctorExpression] = positioned {
    require ~ prepare ~ imports ~ exports ~ define ^^ {
      case r ~ p ~ i ~ e ~ d => FunctorExpression("", r, p, i, d, e)
    }
  }

  lazy val require: PackratParser[List[FunctorImport]] =
    opt("require" ~> rep(importElem)) ^^ (_.getOrElse(Nil))

  lazy val prepare: PackratParser[Option[LocalStatement]] =
    opt("prepare" ~> defineBody)

  lazy val imports: PackratParser[List[FunctorImport]] =
    opt("import" ~> rep(importElem)) ^^ (_.getOrElse(Nil))

  lazy val exports: PackratParser[List[FunctorExport]] =
    opt("export" ~> rep(exportElem)) ^^ (_.getOrElse(Nil))

  lazy val define: PackratParser[Option[LocalStatement]] =
    opt("define" ~> defineBody)

  lazy val importElem: PackratParser[FunctorImport] = positioned(
      variable ~ success(Nil) ~ opt(importLocation) ^^ FunctorImport
    | variableLabel ~ (rep(importAlias) <~ ")") ~ opt(importLocation) ^^ FunctorImport
  )

  lazy val variableLabel: PackratParser[Variable] = positioned {
    identLabel ^^ Variable
  }

  lazy val importAlias: PackratParser[AliasedFeature] = positioned {
    featureNoVar ~ opt(":" ~> variable) ^^ AliasedFeature
  }

  lazy val importLocation: PackratParser[String] =
    "at" ~> atom ^^ (_.value)

  lazy val exportElem: PackratParser[FunctorExport] = positioned {
    exportFeature ~ variable ^^ FunctorExport
  }

  lazy val exportFeature: PackratParser[Expression] = positioned {
    opt(featureNoVar <~ ":") ^^ (_ getOrElse AutoFeature())
  }

  lazy val defineBody: PackratParser[LocalStatement] = positioned {
    declarations ~ opt("in" ~> statement) ^^ {
      case decls ~ optStat =>
        LocalStatement(decls, optStat getOrElse SkipStatement())
    }
  }

  // Skip

  lazy val skipStatement: PackratParser[Statement] = positioned {
    "skip" ^^^ SkipStatement()
  }

  // Operations with precedence

  lazy val expression0: PackratParser[Expression] = (
      positioned((expression1 <~ "=") ~ expression0 ^^ BindExpression)
    | expression1
  )

  // X<-Y   X:=Y   X.Y:=Z   (right-associative)
  lazy val expression1: PackratParser[Expression] = (
      positioned(expression2 ~ ":=" ~ expression1 ^^ BinaryOp)
    | expression2
  )

  // X orelse Y   (right-associative)
  lazy val expression2: PackratParser[Expression] = (
      positioned(expression3 ~ "orelse" ~ expression2 ^^ ShortCircuitBinaryOp)
    | expression3
  )

  // X andthen Y   (right-associative)
  lazy val expression3: PackratParser[Expression] = (
      positioned(expression4 ~ "andthen" ~ expression3 ^^ ShortCircuitBinaryOp)
    | expression4
  )

  // X==Y   X\=Y     X<Y    X=<Y     X>Y    X>=Y   (non-associative)
  // X=:Y   X\=:Y    X<:Y   X=<:Y    X>:Y   X>=:Y
  lazy val expression4: PackratParser[Expression] = (
      positioned(expression5 ~ operator4 ~ expression5 ^^ BinaryOp)
    | expression5
  )

  lazy val operator4 = "==" | "\\=" | "<" | "=<" | ">" | ">="

  // X::Y   X:::Y   (non-associative)
  lazy val expression5: PackratParser[Expression] = expression6

  // X|Y   (right-associative)
  lazy val expression6: PackratParser[Expression] = (
      positioned((expression7 <~ "|") ~ expression6 ^^ cons)
    | expression7
  )

  private def cons(head: Expression, tail: Expression) =
    Record(Constant(OzAtom("|")), List(head, tail))

  // X#Y#...#Z   (mixin)
  lazy val expression7: PackratParser[Expression] = (
      positioned(expression8 ~ rep1("#" ~> expression8) ^^ sharp)
    | expression8
  )

  private def sharp(first: Expression, rest: List[Expression]) =
    Record(Constant(OzAtom("#")), (first :: rest) map expr2recordField)

  // X+Y   X-Y   (left-associative)
  lazy val expression8: PackratParser[Expression] = (
      positioned(expression8 ~ ("+" | "-") ~ expression9 ^^ BinaryOp)
    | expression9
  )

  // X*Y   X/Y    X div Y   X mod Y   (left-associative)
  lazy val expression9: PackratParser[Expression] = (
      positioned(expression9 ~ operator9 ~ expression10 ^^ BinaryOp)
    | expression10
  )

  lazy val operator9 = "*" | "/" | "div" | "mod"

  // X,Y   (right-associative)
  lazy val expression10: PackratParser[Expression] = expression11

  // ~X   (prefix)
  lazy val expression11: PackratParser[Expression] = (
      positioned("~" ~ expression11 ^^ UnaryOp)
    | expression12
  )

  // X.Y   X^Y   (left-associative)
  lazy val expression12: PackratParser[Expression] = (
      positioned(expression12 ~ "." ~ expression13 ^^ BinaryOp)
    | expression13
  )

  // @X   !!X   (prefix)
  lazy val expression13: PackratParser[Expression] = (
      positioned("@" ~ expression13 ^^ UnaryOp)
    | expression14
  )

  // elementary
  lazy val expression14: PackratParser[Expression] = (
      "local" ~> inExpression <~ "end"
    | "(" ~> inExpression <~ ")"
    | procExpression
    | funExpression
    | callExpression
    | ifExpression
    | caseExpression
    | threadExpression
    | functorExpression
    | trivialExpression
    | recordExpression
    | listExpression
  )

  // Trivial expressions

  lazy val trivialExpression: PackratParser[Expression] = (
      variable
    | positioned("!!" ~> variable ^^ EscapedVariable)
    | unboundExpression
    | positioned(integerConst ^^ Constant)
    | positioned(floatConst ^^ Constant)
    | positioned(atomLike ^^ Constant)
  )

  lazy val unboundExpression: PackratParser[UnboundExpression] =
    positioned("_" ^^^ UnboundExpression())

  lazy val integerConst: PackratParser[OzInt] =
    numericLit ^^ (chars => OzInt(chars.toInt))

  lazy val floatConst: PackratParser[OzFloat] =
    floatLit ^^ (chars => OzFloat(chars.toInt))

  lazy val atomLike: PackratParser[OzLiteral] = (
      "true" ^^^ True()
    | "false" ^^^ False()
    | "unit" ^^^ UnitVal()
    | atom
  )

  lazy val atom: PackratParser[OzAtom] =
    atomLit ^^ (chars => OzAtom(chars))

  lazy val variable: PackratParser[Variable] =
    positioned(ident ^^ (chars => Variable(chars)))

  lazy val featureNoVar = positioned(featureNoVarConst ^^ Constant)

  lazy val featureNoVarConst: PackratParser[OzFeature] =
    integerConst | atomLike

  // Record expressions

  lazy val recordExpression: PackratParser[Expression] =
    positioned(recordLabel ~ recordFields <~ ")" ^^ Record)

  lazy val recordLabel: PackratParser[Expression] = (
      positioned(atomLitLabel ^^ (chars => Constant(OzAtom(chars))))
    | positioned(identLabel ^^ Variable)
  )

  lazy val recordFields: PackratParser[List[RecordField]] =
    rep(recordField)

  lazy val recordField: PackratParser[RecordField] = (
      positioned(expression ~ (":" ~> expression) ^^ RecordField)
    | positioned(expression ^^ (expr => RecordField(AutoFeature(), expr)))
  )

  // List expressions

  lazy val listExpression: PackratParser[Expression] =
    positioned("[" ~> (expression+) <~ "]" ^^ exprListToListExpr)

  def exprListToListExpr(elems: List[Expression]): Expression =
    if (elems.isEmpty) Constant(OzAtom("nil"))
    else {
      Record(Constant(OzAtom("|")),
          List(elems.head, exprListToListExpr(elems.tail)))
    }
}
