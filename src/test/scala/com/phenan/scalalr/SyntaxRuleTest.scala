package com.phenan.scalalr

import org.scalatest._

/**
  * Created by @phenan on 2016/12/12.
  */
class SyntaxRuleTest extends FunSuite with Matchers {

  import CommandLineApplication._

  test("expression") {
    val syntax = simpleSyntax

    syntax.expressions(NonTerminalImpl("S")) shouldBe List(List(nonTerminalSymbol("T")))
    syntax.expressions(NonTerminalImpl("T")) shouldBe List(List(nonTerminalSymbol("M")), List(nonTerminalSymbol("N")))
    syntax.expressions(NonTerminalImpl("M")) shouldBe List(List(nonTerminalSymbol("T"), keywordSymbol("mul"), nonTerminalSymbol("N")))
    syntax.expressions(NonTerminalImpl("N")) shouldBe List(List(literalTokenSymbol("int", "Int")))
  }

  test("first") {
    val syntax = simpleSyntax

    syntax.lookupFirst(List(nonTerminalSymbol("S"))) shouldBe Set(Terminal(LiteralTokenImpl("int", "Int")))
    syntax.lookupFirst(List(nonTerminalSymbol("T"))) shouldBe Set(Terminal(LiteralTokenImpl("int", "Int")))
    syntax.lookupFirst(List(nonTerminalSymbol("T"), keywordSymbol("mul"), nonTerminalSymbol("N"))) shouldBe Set(Terminal(LiteralTokenImpl("int", "Int")))
    syntax.lookupFirst(List(nonTerminalSymbol("M"))) shouldBe Set(Terminal(LiteralTokenImpl("int", "Int")))
    syntax.lookupFirst(List(keywordSymbol("mul"), nonTerminalSymbol("N"))) shouldBe Set(Terminal(Keyword("mul")))
  }


  def simpleSyntax = SyntaxRule(List("Simple"), NonTerminalImpl("S"), List(
    DerivationRule(NonTerminalImpl("S"), List(nonTerminalSymbol("T"))),
    BranchRule(NonTerminalImpl("T"), List(NonTerminalImpl("M"), NonTerminalImpl("N"))),
    DerivationRule(NonTerminalImpl("M"), List(nonTerminalSymbol("T"), keywordSymbol("mul"), nonTerminalSymbol("N"))),
    DerivationRule(NonTerminalImpl("N"), List(literalTokenSymbol("int", "Int")))
  ))
}

