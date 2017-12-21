package com.phenan.scalalr

import org.scalatest._

/**
  * Created by @phenan on 2016/12/12.
  */
class SyntaxTest extends FunSuite with Matchers {

  test("expression") {
    val syntax = simpleSyntax

    syntax.expressions(NonTerminal("S")) shouldBe List(List(NonTerminal("T")))
    syntax.expressions(NonTerminal("T")) shouldBe List(List(NonTerminal("M")), List(NonTerminal("N")))
    syntax.expressions(NonTerminal("M")) shouldBe List(List(NonTerminal("T"), Keyword("mul"), NonTerminal("N")))
    syntax.expressions(NonTerminal("N")) shouldBe List(List(IntLiteral))
  }

  test("first") {
    val syntax = simpleSyntax

    syntax.lookupFirst(List(NonTerminal("S"))) shouldBe Set(IntLiteral)
    syntax.lookupFirst(List(NonTerminal("T"))) shouldBe Set(IntLiteral)
    syntax.lookupFirst(List(NonTerminal("T"), Keyword("mul"), NonTerminal("N"))) shouldBe Set(IntLiteral)
    syntax.lookupFirst(List(NonTerminal("M"))) shouldBe Set(IntLiteral)
    syntax.lookupFirst(List(Keyword("mul"), NonTerminal("N"))) shouldBe Set(Keyword("mul"))
  }


  def simpleSyntax = Syntax("Simple", NonTerminal("S"), List(
    DerivationRule(NonTerminal("S"), List(NonTerminal("T"))),
    BranchRule(NonTerminal("T"), List(NonTerminal("M"), NonTerminal("N"))),
    DerivationRule(NonTerminal("M"), List(NonTerminal("T"), Keyword("mul"), NonTerminal("N"))),
    DerivationRule(NonTerminal("N"), List(IntLiteral))
  ))
}

