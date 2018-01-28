package com.phenan.scalalr
package macroimpl

import shared._

trait MacroSyntaxRuleModule {
  this: TypingModule with SyntaxRuleModule with MacroModule =>

  import c.universe._

  case class NonTerminalImpl (ntType: GenericType)
  case class LiteralTokenImpl (literalType: GenericType)

  sealed trait SemanticActionImpl

  case object Inheritance extends SemanticActionImpl
  case object LiteralRef extends SemanticActionImpl
  case class ObjectRef (termName: TermName) extends SemanticActionImpl
  case class ConstructorCall (typeName: TypeName, parameterIndexes: Map[NonTerminal, (Int, Int)]) extends SemanticActionImpl
  case class FunctionCall (functionName: TermName, parameterIndexes: Map[NonTerminal, (Int, Int)]) extends SemanticActionImpl

  override type NonTerminal = NonTerminalImpl
  override type LiteralToken = LiteralTokenImpl
  override type SemanticAction = SemanticActionImpl
}
