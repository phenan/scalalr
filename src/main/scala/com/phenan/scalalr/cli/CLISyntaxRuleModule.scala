package com.phenan.scalalr
package cli

import shared._

trait CLISyntaxRuleModule extends SyntaxRuleModule {

  case class NonTerminalImpl (name: String)
  case class LiteralTokenImpl (identifier: String, litType: String)

  sealed trait SemanticActionImpl
  case object Branch extends SemanticActionImpl
  case object Derivation extends SemanticActionImpl

  override type NonTerminal = NonTerminalImpl
  override type LiteralToken = LiteralTokenImpl
  override type SemanticAction = SemanticActionImpl

  def nonTerminalSymbol (name: String): Symbol = Symbol(NonTerminalImpl(name))
  def keywordSymbol (name: String): Symbol = Symbol(Terminal(Keyword(name)))
  def literalTokenSymbol (identifier: String, litType: String): Symbol = Symbol(Terminal(LiteralTokenImpl(identifier, litType)))
}
