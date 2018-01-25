package com.phenan.scalalr
package cli

import shared._

import java.io._

import scala.util.Random

import scala.{Console => Stdio}

object CLIApplication extends CLIApplicationModule
  with CLIOptionParserModule with SyntaxFileParserModule with CLISyntaxRuleModule
  with SyntaxRuleModule with LALRAutomatonModule with CodeGeneratorModule

trait CLIApplicationModule {
  self: SyntaxFileParserModule with CLIOptionParserModule with CLISyntaxRuleModule with SyntaxRuleModule with CodeGeneratorModule with LALRAutomatonModule =>

  def applicationMain (args: Array[String]): Unit = optionParser.parse(args, Config()) match {
    case Some(config) if config.syntaxFile != null => run(config)
    case _ => optionParser.showUsage()
  }

  def run (config: Config): Unit = {
    SyntaxParsers.runParser(config.syntaxFile) match {
      case Right(syntax) =>
        if (config.printFlag) printGeneratedCode(syntax)
        else writeGeneratedCode(syntax, config.directory)
      case Left(msg) =>
        Stdio.err.println(s"invalid syntax file : ${config.syntaxFile}\n  $msg")
    }
  }

  def printGeneratedCode (syntax: SyntaxRule): Unit = {
    val gen = CodeGenerator(LALRAutomaton(syntax))
    println("/***********************/")
    println(gen.generateCode(gen.astDataTypeDefinitions))
    println("\n/***********************/\n")
    println(gen.generateCode(gen.program))
  }

  def writeGeneratedCode (syntax: SyntaxRule, directory: Option[File]): Unit = {
    val dir = directory.getOrElse(new File("."))
    val dslFile = new File(dir, syntax.qualifiedName.mkString("/") + ".scala")
    val parent = dslFile.getParentFile
    val astFile = new File(parent, "ASTs.scala")
    parent.mkdirs()

    val gen = CodeGenerator(LALRAutomaton(syntax))
    val writer1 = new BufferedWriter(new FileWriter(astFile))
    if (syntax.qualifiedName.init.nonEmpty) {
      writer1.write(s"package ${syntax.qualifiedName.init.mkString(".")}")
      writer1.newLine()
    }
    writer1.write(gen.generateCode(gen.astDataTypeDefinitions))
    writer1.close()

    val writer2 = new BufferedWriter(new FileWriter(dslFile))
    if (syntax.qualifiedName.init.nonEmpty) {
      writer2.write(s"package ${syntax.qualifiedName.init.mkString(".")}")
      writer2.newLine()
      writer2.newLine()
      writer2.write("import com.phenan.scalalr._")
      writer2.newLine()
    }
    writer2.write(gen.generateCode(gen.program))
    writer2.close()
  }


  override type GeneratedCode = String

  override val output: Output = StringOutput

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
