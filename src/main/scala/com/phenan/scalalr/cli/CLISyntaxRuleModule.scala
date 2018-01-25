package com.phenan.scalalr
package cli

import shared._

trait CLISyntaxRuleModule extends SyntaxRuleModule {

  case class NonTerminalImpl (name: String)
  case class LiteralTokenImpl (identifier: String, litType: String)

  override type NonTerminal = NonTerminalImpl
  override type LiteralToken = LiteralTokenImpl

  def nonTerminalSymbol (name: String): Symbol = Symbol(NonTerminalImpl(name))
  def keywordSymbol (name: String): Symbol = Symbol(Terminal(Keyword(name)))
  def literalTokenSymbol (identifier: String, litType: String): Symbol = Symbol(Terminal(LiteralTokenImpl(identifier, litType)))
}
