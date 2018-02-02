package com.phenan.scalalr
package macroimpl

import shared._

trait TreeGeneratorModule {
  this: CodeGeneratorModule with TyperModule with SyntaxGeneratorModule with SyntaxInfoModule with SyntaxRuleModule with MacroModule =>

  import c.universe._

  override type GeneratedCode = List[Tree]

  override lazy val output: TreeOutput = new TreeOutput(typer)

  class TreeOutput (typer: Typer) extends Output {
    override type Type = Tree
    override type Parameter = ValDef
    override type TypeParameter = TypeDef
    override type MemberDef = Tree
    override type Expr = Tree

    def generateProgram (modules: List[Tree]): GeneratedCode = modules

    def generateUniqueName: String = c.freshName("ScaLALR$")

    def literalIdentifier (lit: LiteralToken): Option[String] = None

    def simpleType (typeName: String): Type = typer.stringToTypeTree(typeName)
    def objectType (objectName: String): Type = tq"${typer.stringToQualifiedTerm(objectName)}.type"
    def nonTerminalType (nt: NonTerminal): Type = typer.unchecked(nt.ntType)
    def literalType (lit: LiteralToken): Type = typer.unchecked(lit.literalType)

    def tuple2Type (v1: Type, v2: Type): Type = tq"($v1, $v2)"
    def functionType (left: Type, right: Type): Type = tq"$left => $right"
    def parameterizedType (genName: String, args: List[Type]): Type = tq"${typer.stringToTypeTree(genName)}[..$args]"

    def parameter (name: String, paramType: Type): Parameter = ValDef(Modifiers(Flag.PARAM), TermName(name), paramType, EmptyTree)
    def unusedParameter (paramType: Type): Parameter = ValDef(Modifiers(Flag.PARAM), TermName(generateUniqueName), paramType, EmptyTree)

    def typeParameter (name: String): TypeParameter = TypeDef(Modifiers(Flag.PARAM), TypeName(name), Nil, TypeBoundsTree(EmptyTree, EmptyTree))
    def typeParameter (name: String, bound: Type): TypeParameter = TypeDef(Modifiers(Flag.PARAM), TypeName(name), Nil, TypeBoundsTree(EmptyTree, bound))

    def sealedTraitDef (name: String, superType: Option[Type]): MemberDef = superType match {
      case Some(sup) => q"sealed trait ${TypeName(name)} extends $sup"
      case None      => q"sealed trait ${TypeName(name)}"
    }

    def caseClassDef (name: String, typeParams: List[TypeParameter], params: List[Parameter], superType: Option[Type]): MemberDef = superType match {
      case Some(sup) => q"case class ${TypeName(name)} [..$typeParams] (..$params) extends $sup"
      case None      => q"case class ${TypeName(name)} [..$typeParams] (..$params)"
    }

    def caseObjectDef (name: String): MemberDef = q"case object ${TermName(name)}"

    def lazyValDef (name: String, valType: Type, value: Expr): MemberDef = q"lazy val ${TermName(name)}: $valType = $value"

    def functionDef (name: String, typeParams: List[TypeParameter], parameters: List[Parameter], implicitParams: List[Parameter], returnType: Type, body: Expr): MemberDef = {
      if (parameters.isEmpty) q"def ${TermName(name)} [..$typeParams] (implicit ..$implicitParams): $returnType = $body"
      else q"def ${TermName(name)} [..$typeParams] (..$parameters)(implicit ..$implicitParams): $returnType = $body"
    }

    def implicitFunctionDef (typeParams: List[TypeParameter], parameters: List[Parameter], implicitParams: List[Parameter], returnType: Type, body: Expr): MemberDef = {
      if (parameters.isEmpty) q"implicit def ${TermName(generateUniqueName)} [..$typeParams] (implicit ..$implicitParams): $returnType = $body"
      else q"implicit def ${TermName(generateUniqueName)} [..$typeParams] (..$parameters)(implicit ..$implicitParams): $returnType = $body"
    }

    def implicitClassDef (typeParams: List[TypeParameter], parameter: Parameter, implicitParams: List[Parameter], members: List[MemberDef]): MemberDef = {
      q"implicit class ${TypeName(generateUniqueName)} [..$typeParams] ($parameter)(implicit ..$implicitParams) { ..$members }"
    }

    def objectRef (objectName: String): Expr = typer.stringToQualifiedTerm(objectName)

    def methodCall (receiver: Expr, methodName: String, typeArgs: List[Type], args: List[Expr]): Expr = {
      q"$receiver.${TermName(methodName)}[..$typeArgs](..$args)"
    }

    def fieldRef (receiver: Expr, fieldName: String): Expr = {
      q"$receiver.${TermName(fieldName)}"
    }

    def callApply (receiver: Expr, typeArgs: List[Type], args: List[Expr]): Expr = {
      q"$receiver[..$typeArgs](..$args)"
    }

    def lambda (parameters: List[Parameter], body: Expr): Expr = {
      q"(..$parameters) => $body"
    }

    def constructAST (rule: Rule, args: List[Expr]): Expr = rule.action match {
      case Inheritance if args.lengthCompare(1) == 0 => args.head
      case LiteralRef if args.lengthCompare(1) == 0 => args.head
      case ObjectRef(name) if args.isEmpty => name
      case ConstructorCall(name, correspondence) => q"new $name(...${correspondence(args)})"
      case FunctionCall(name, correspondence) => q"$name(...${correspondence(args)})"
      case ListCons if args.lengthCompare(2) == 0 => q"${args.head} +: ${args.tail.head}"
      case ConstructSeq if args.lengthCompare(2) == 0 => q"${args.head} +: ${args.tail.head}.toSeq"
      case ConstructSeqTail if args.lengthCompare(2) == 0 => q"com.phenan.scalalr.internal.ConsSeqTail(${args.head}, ${args.tail.head})"
      case SingleList if args.lengthCompare(1) == 0 => q"scala.collection.immutable.List($args.head)"
      case SingleSeqTail if args.lengthCompare(1) == 0 => q"com.phenan.scalalr.internal.ConsSeqTail(${args.head}, com.phenan.scalalr.internal.SeqTail.empty)"
    }
  }
}
