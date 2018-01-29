package com.phenan.scalalr

import cli._

import org.scalatest._

/**
  * Created by @phenan on 2016/12/12.
  */
class SyntaxTest extends FunSuite with Matchers {

  import CLIApplication._

  test("expression") {
    val syntax = simpleSyntax

    syntax.expressions(NonTerminalImpl("S")) shouldBe List(derivation_S_T)
    syntax.expressions(NonTerminalImpl("T")) shouldBe List(branch_T_M, branch_T_N)
    syntax.expressions(NonTerminalImpl("M")) shouldBe List(derivation_M_mul)
    syntax.expressions(NonTerminalImpl("N")) shouldBe List(derivation_N_int)
  }

  test("first") {
    val syntax = simpleSyntax

    syntax.lookupFirst(List(nonTerminalSymbol("S"))) shouldBe Set(Terminal(LiteralTokenImpl(Some("int"), "Int")))
    syntax.lookupFirst(List(nonTerminalSymbol("T"))) shouldBe Set(Terminal(LiteralTokenImpl(Some("int"), "Int")))
    syntax.lookupFirst(List(nonTerminalSymbol("T"), keywordSymbol("mul"), nonTerminalSymbol("N"))) shouldBe Set(Terminal(LiteralTokenImpl(Some("int"), "Int")))
    syntax.lookupFirst(List(nonTerminalSymbol("M"))) shouldBe Set(Terminal(LiteralTokenImpl(Some("int"), "Int")))
    syntax.lookupFirst(List(keywordSymbol("mul"), nonTerminalSymbol("N"))) shouldBe Set(Terminal(Keyword("mul")))
  }


  def simpleSyntax = Syntax(NonTerminalImpl("S"), List(derivation_S_T, branch_T_M, branch_T_N, derivation_M_mul, derivation_N_int))

  private lazy val derivation_S_T: Rule = Rule(NonTerminalImpl("S"), List(nonTerminalSymbol("T")), Derivation)
  private lazy val branch_T_M: Rule = Rule(NonTerminalImpl("T"), List(nonTerminalSymbol("M")), Branch)
  private lazy val branch_T_N: Rule = Rule(NonTerminalImpl("T"), List(nonTerminalSymbol("N")), Branch)
  private lazy val derivation_M_mul: Rule = Rule(NonTerminalImpl("M"), List(nonTerminalSymbol("T"), keywordSymbol("mul"), nonTerminalSymbol("N")), Derivation)
  private lazy val derivation_N_int: Rule = Rule(NonTerminalImpl("N"), List(literalTokenSymbol("int", "Int")), Derivation)
}

