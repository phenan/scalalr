package com.phenan.scalalr
package cli

import shared._
import java.io._

import scala.util.Random
import scala.{Console => Stdio}

trait ScalaCodeGeneratorModule {
  this: ASTDataTypeWriterModule with CodeGeneratorModule with CLISyntaxRuleModule with SyntaxRuleModule with LALRAutomatonModule =>

  def printGeneratedCode (qualifiedName: List[String], syntax: SyntaxRule): Unit = {
    val writer = new PrintWriter(Stdio.out)
    writeASTDataType(qualifiedName, syntax, writer)
    writeGeneratedDefinitions(qualifiedName, syntax, writer)
  }

  def writeGeneratedCode (qualifiedName: List[String], syntax: SyntaxRule, directory: Option[File]): Unit = {
    val dir = directory.getOrElse(new File("."))
    val dslFile = new File(dir, qualifiedName.mkString("/") + ".scala")
    val parent = dslFile.getParentFile
    val astFile = new File(parent, "ASTs.scala")
    parent.mkdirs()

    val writer1 = new PrintWriter(astFile)
    writeASTDataType(qualifiedName, syntax, writer1)
    writer1.close()

    val writer2 = new PrintWriter(dslFile)
    writeGeneratedDefinitions(qualifiedName, syntax, writer2)
    writer2.close()
  }

  def writeGeneratedDefinitions (qualifiedName: List[String], syntax: SyntaxRule, writer: PrintWriter): Unit = {
    val gen = CodeGenerator(LALRAutomaton(syntax))
    if (qualifiedName.lengthCompare(1) > 0) {
      writer.println(s"package ${qualifiedName.init.mkString(".")}")
    }
    writer.println(gen.generateCode(output.moduleDefinition(qualifiedName.last, gen.generatedDefinitions)))
  }

  override type GeneratedCode = String

  override val output: StringOutput.type = StringOutput

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

    def literalIdentifier (lit: LiteralToken): Option[String] = lit.identifier

    def simpleType (typeName: String): Type = typeName
    def objectType (objectName: String): Type = objectName + ".type"
    def nonTerminalType (nt: NonTerminal): Type = nt.name
    def literalType (lit: LiteralToken): String = lit.litType

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

    def sealedTraitDef (name: String, superType: Option[Type]): MemberDef = s => {
      s"${s.newLine}sealed trait $name${extendsClause(superType)}"
    }

    def caseClassDef (name: String, typeParams: List[TypeParameter], params: List[Parameter], superType: Option[Type]): MemberDef = s => {
      s"${s.newLine}case class $name ${typeParameters(typeParams)} ${parameters(params)}${extendsClause(superType)}"
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

    def constructAST (rule: Rule, args: List[Expr]): Expr = s => rule.action match {
      case Branch if args.lengthCompare(1) == 0 => args.head(s)
      case Derivation => s"${rule.left.name}${arguments(args)(s)}"
      case _ => throw new RuntimeException("branch rule should take only one argument")
    }

    private def extendsClause (superType: Option[Type]): String = superType match {
      case Some(t) => s" extends $t"
      case None    => ""
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
