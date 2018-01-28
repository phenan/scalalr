package com.phenan.scalalr
package macroimpl

import shared._

import java.util.regex._

trait ASTGeneratorModule {
  this: CodeGeneratorModule with TypingModule with MacroSyntaxRuleModule with SyntaxRuleModule with MacroModule =>

  import c.universe._

  override type GeneratedCode = List[Tree]

  override val output: ASTOutput.type = ASTOutput

  object ASTOutput extends Output {
    override type Type = Tree
    override type Parameter = ValDef
    override type TypeParameter = TypeDef
    override type MemberDef = Tree
    override type Expr = Tree

    def generateProgram (modules: List[Tree]): GeneratedCode = modules

    def generateUniqueName: String = c.freshName("ScaLALR$")

    def literalIdentifier (lit: LiteralToken): Option[String] = None

    def simpleType (typeName: String): Type = stringToType(typeName)
    def objectType (objectName: String): Type = tq"${stringToQualifiedTerm(objectName)}.type"
    def nonTerminalType (nt: NonTerminal): Type = genericType(nt.ntType)
    def literalType (lit: LiteralToken): Type = genericType(lit.literalType)

    def tuple2Type (v1: Type, v2: Type): Type = tq"($v1, $v2)"
    def functionType (left: Type, right: Type): Type = tq"$left => $right"
    def parameterizedType (genName: String, args: List[Type]): Type = tq"${stringToType(genName)}[..$args]"

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

    def objectRef (objectName: String): Expr = stringToQualifiedTerm(objectName)

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
      case ObjectRef(name) if args.isEmpty => q"$name"
      case ConstructorCall(name, correspondence) => q"new $name(...${correspondence(args)})"
      case FunctionCall(name, correspondence) => q"$name(...${correspondence(args)})"
    }

    private def genericType (t: GenericType): Type = t match {
      case ParameterizedType(clazz, args) => tq"${typeConstructor(clazz)}[..${args.map(genericType)}]"
      case SimpleType(clazz) => typeConstructor(clazz)
      case SingletonType(obj) => tq"${genericObject(obj)}.type"
    }

    private def stringToType (string: String): Type = {
      val dot = string.lastIndexOf('.')
      val prefix = string.take(dot)
      val postfix = string.drop(dot + 1)

      if (prefix.isEmpty) tq"${TypeName(postfix)}"
      else tq"${stringToQualifiedTerm(prefix)}.${TypeName(postfix)}"
    }

    private def stringToQualifiedTerm (string: String): Tree = {
      val terms = string.split(Pattern.quote(".")).map(TermName(_))
      terms.tail.foldLeft [Tree] (q"${terms.head}") { (left, term) => q"$left.$term" }
    }

    private def typeConstructor (clazz: GenericClass): Tree = clazz match {
      case DSLClass(outer, name) => tq"$outer.$name"
      case ScalaClass(t) => stringToType(t.typeSymbol.fullName)              // 頭悪い方法な気がする
    }

    private def genericObject (obj: GenericObject): Tree = obj match {
      case DSLObject(outer, name) => q"$outer.$name"
      case ScalaObject(t) => stringToQualifiedTerm(t.termSymbol.fullName)    // 頭悪い方法な気がする
    }
  }
}
