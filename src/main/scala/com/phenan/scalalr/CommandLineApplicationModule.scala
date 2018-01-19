package com.phenan.scalalr

import java.io._

import scala.util.parsing.combinator.JavaTokenParsers
import shapeless._
import shapeless.ops.coproduct.Inject

import scala.util.Random

trait CommandLineApplicationModule {
  self: SyntaxRuleModule with CodeGeneratorModule =>

  override type NonTerminal = NonTerminalImpl
  override type LiteralToken = LiteralTokenImpl
  override type GeneratedCode = String

  override val output: Output = StringOutput

  case class NonTerminalImpl (name: String)
  case class LiteralTokenImpl (identifier: String, litType: String)

  def nonTerminalSymbol (name: String): Symbol = Symbol(NonTerminalImpl(name))
  def keywordSymbol (name: String): Symbol = Symbol(Terminal(Keyword(name)))
  def literalTokenSymbol (identifier: String, litType: String): Symbol = Symbol(Terminal(LiteralTokenImpl(identifier, litType)))

  object SyntaxParsers extends JavaTokenParsers {

    def runParser (file: File): Either[String, SyntaxRule] = {
      val reader = new BufferedReader(new FileReader(file))
      val parseResult = parseAll(syntax, reader)
      reader.close()
      parseResult match {
        case Success(r, _)   => Right(r)
        case NoSuccess(m, _) => Left(m)
      }
    }

    def syntax: Parser[SyntaxRule] = "syntax" ~> rep1sep(ident, ".") ~ ("(" ~> nonTerminal <~ ")" ) ~ ("{" ~> rule.* <~ "}" ) ^^ {
      case name ~ start ~ rules => SyntaxRule(name, start, rules)
    }

    def rule: Parser[Rule] = branch | derivation

    def branch: Parser[BranchRule] = ( nonTerminal <~ "=" ) ~ rep1sep(nonTerminal, "|") <~ ";" ^^ {
      case left ~ right => BranchRule(left, right)
    }

    def derivation: Parser[DerivationRule] = ( nonTerminal <~ "=" ) ~ choice[Symbol](terminal, nonTerminal).+ <~ ";" ^^ {
      case left ~ right => DerivationRule(left, right)
    }

    def nonTerminal: Parser[NonTerminal] = not(id | int) ~> ident ^^ { id => NonTerminalImpl(id.capitalize) }

    def terminal: Parser[Terminal] = choice[Terminal](id, int, keyword)

    def id: Parser[LiteralToken] = "id" ^^^ LiteralTokenImpl("id", "String")

    def int: Parser[LiteralToken] = "int" ^^^ LiteralTokenImpl("int", "Int")

    def keyword: Parser[Keyword] = stringLiteral ^^ { lit => Keyword(lit.substring(1, lit.length - 1)) }

    private def choice[R <: Coproduct]: Choice[R] = new Choice[R]

    private class Choice [R <: Coproduct] {
      def apply [T1, T2] (p1: => Parser[T1], p2: => Parser[T2]) (implicit inj1: Inject[R, T1], inj2: Inject[R, T2]): Parser[R] = {
        p1.map(inj1(_)) | p2.map(inj2(_))
      }
      def apply [T1, T2, T3] (p1: => Parser[T1], p2: => Parser[T2], p3: => Parser[T3]) (implicit inj1: Inject[R, T1], inj2: Inject[R, T2], inj3: Inject[R, T3]): Parser[R] = {
        p1.map(inj1(_)) | p2.map(inj2(_)) | p3.map(inj3(_))
      }
    }
  }

  object StringOutput extends Output {
    case class OutputState (indentLevel: Int) {
      lazy val indent: OutputState = OutputState(indentLevel + 1)
      lazy val newLine: String = "\n" + indentString
      lazy val indentString: String = (0 until indentLevel).map(_ => "  ").mkString
    }

    type OutputBuilder = OutputState => String

    type MemberDef = OutputBuilder

    type Type = String
    type Parameter = String
    type TypeParameter = String
    type Expr = OutputBuilder

    Random.setSeed(System.currentTimeMillis())

    def generateProgram (modules: List[MemberDef]): GeneratedCode = modules.map(_(OutputState(0))).mkString

    def generateUniqueName: String = "ScaLALR$" + Random.nextInt.abs

    def literalIdentifier (lit: LiteralToken): String = lit.identifier

    def simpleType (typeName: String): Type = typeName
    def objectType (objectName: String): Type = objectName + ".type"
    def nonTerminalType (nt: NonTerminal): Type = nt.name
    def literalType (lit: LiteralToken): Type = lit.litType

    def tuple2Type (v1: Type, v2: Type): Type = s"($v1, $v2)"
    def functionType (left: Type, right: Type): Type = s"$left => $right"
    def parameterizedType (genName: String, args: List[Type]): Type = s"$genName${typeArguments(args)}"

    def parameter (name: String, paramType: Type): Parameter = s"$name: $paramType"
    def unusedParameter (paramType: Type): Parameter = s"_ : $paramType"

    def typeParameter (name: String): TypeParameter = name
    def typeParameter (name: String, bound: Type): TypeParameter = s"$name <: $bound"

    def moduleDefinition (moduleName: String, members: List[MemberDef]): MemberDef = s => {
      s"${s.newLine}object $moduleName {${members.map(_(s.indent)).mkString}${s.newLine}}"
    }

    def branchDataTypeDef (nt: NonTerminal, superType: Option[NonTerminal]): MemberDef = s => superType match {
      case Some(sup) => s"${s.newLine}sealed trait ${nt.name} extends ${sup.name}"
      case None      => s"${s.newLine}sealed trait ${nt.name}"
    }

    def derivationDateTypeDef (nt: NonTerminal, params: List[Parameter], superType: Option[NonTerminal]): MemberDef = s => superType match {
      case Some(sup) => s"${s.newLine}case class ${nt.name}${parameters(params)} extends ${sup.name}"
      case None      => s"${s.newLine}case class ${nt.name}${parameters(params)}"
    }

    def caseClassDef (name: String, typeParams: List[TypeParameter], params: List[Parameter]): MemberDef = s => {
      s"${s.newLine}case class $name ${typeParameters(typeParams)} ${parameters(params)}"
    }

    def caseObjectDef (name: String): MemberDef = s => s"${s.newLine}case object $name"

    def lazyValDef (name: String, valType: Type, value: Expr): MemberDef = s => s"${s.newLine}lazy val $name : $valType = ${value(s)}"

    def functionDef (name: String, typeParams: List[String], params: List[Parameter], implicitParams: List[Parameter], returnType: Type, body: Expr): MemberDef = s => {
      s"${s.newLine}def $name ${typeParameters(typeParams)}${parameters(params)}${implicitParameters(implicitParams)}: $returnType = ${body(s)}"
    }

    def implicitFunctionDef (typeParams: List[String], params: List[Parameter], implicitParams: List[Parameter], returnType: Type, body: Expr): MemberDef = s => {
      s"${s.newLine}implicit def $generateUniqueName ${typeParameters(typeParams)}${parameters(params)}${implicitParameters(implicitParams)}: $returnType = ${body(s)}"
    }

    def implicitClassDef (typeParams: List[String], param: Parameter, implicitParams: List[Parameter], members: List[MemberDef]): MemberDef = s => {
      s"${s.newLine}implicit class $generateUniqueName ${typeParameters(typeParams)}($param)${implicitParameters(implicitParams)} {${members.map(_(s.indent)).mkString}${s.newLine}}"
    }

    def objectRef (objectName: String): Expr = _ => objectName
    def methodCall (receiver: Expr, methodName: String, typeArgs: List[Type], args: List[Expr]): Expr = s => s"${receiver(s)}.$methodName${typeArguments(typeArgs)}${arguments(args)(s)}"
    def fieldRef (receiver: Expr, fieldName: String): Expr = s => s"${receiver(s)}.$fieldName"
    def callApply (receiver: Expr, typeArgs: List[Type], args: List[Expr]): Expr = s => s"${receiver(s)}${typeArguments(typeArgs)}${arguments(args)(s)}"
    def lambda (params: List[Parameter], body: Expr): Expr = s => s"{ ${parameters(params)} => ${body(s)} }"

    def constructAST (nonTerminal: NonTerminal, args: List[Expr]): Expr = s => {
      s"${nonTerminal.name}${arguments(args)(s)}"
    }

    private def typeParameters (typeParams: List[TypeParameter]): String = {
      if (typeParams.nonEmpty) typeParams.mkString("[", ", ", "]")
      else ""
    }

    private def typeArguments (typeArgs: List[Type]): String = {
      if (typeArgs.nonEmpty) typeArgs.mkString("[", ", ", "]")
      else ""
    }

    private def parameters (params: List[Parameter]): String = {
      if (params.nonEmpty) params.mkString("(", ", ", ")")
      else ""
    }

    private def implicitParameters (params: List[Parameter]): String = {
      if (params.nonEmpty) params.mkString("(implicit ", ", ", ")")
      else ""
    }

    private def arguments (args: List[Expr]): OutputBuilder = s => {
      args.map(_(s)).mkString("(", ", ", ")")
    }
  }
}
