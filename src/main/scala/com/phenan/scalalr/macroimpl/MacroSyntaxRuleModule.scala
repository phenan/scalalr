package com.phenan.scalalr
package macroimpl

import shared._

trait MacroSyntaxRuleModule {
  this: TypingModule with SyntaxRuleModule =>

  case class NonTerminalImpl (ntType: GenericType)

  override type NonTerminal = NonTerminalImpl
  //override type LiteralToken = this.type
}
