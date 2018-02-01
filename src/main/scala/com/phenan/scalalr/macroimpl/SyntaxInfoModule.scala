package com.phenan.scalalr.macroimpl

trait SyntaxInfoModule {
  this: MacroModule =>

  import c.universe._

  sealed trait SemanticActionImpl
  case object Inheritance extends SemanticActionImpl
  case object LiteralRef extends SemanticActionImpl
  case class ObjectRef (termName: TermName) extends SemanticActionImpl
  case class ConstructorCall (typeName: TypeName, parameterCorrespondence: List[Tree] => List[List[Tree]]) extends SemanticActionImpl
  case class FunctionCall (functionName: TermName, parameterCorrespondence: List[Tree] => List[List[Tree]]) extends SemanticActionImpl

  case class SyntaxInfo (returnType: Tree, operators: List[List[String]], operandTypes: List[Tree], semantics: SemanticActionImpl)
}
