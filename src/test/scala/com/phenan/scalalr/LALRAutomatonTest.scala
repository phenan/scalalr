package com.phenan.scalalr

import org.scalatest._

/**
  * Created by @phenan on 2016/12/12.
  */
class LALRAutomatonTest extends FunSuite with Matchers {
  import CommandLineApplication._

  test ("LALR automaton") {
    val automaton = LALRAutomaton(simpleSyntax)

    val node0 = LRClosure(Map(
      LRItem(NonTerminalImpl("S"), List(nonTerminalSymbol("T")), List(nonTerminalSymbol("T"))) -> Set(Terminal.eoi),
      LRItem(NonTerminalImpl("T"), List(nonTerminalSymbol("M")), List(nonTerminalSymbol("M"))) -> Set(Terminal.eoi, Terminal(Keyword("mul"))),
      LRItem(NonTerminalImpl("T"), List(nonTerminalSymbol("N")), List(nonTerminalSymbol("N"))) -> Set(Terminal.eoi, Terminal(Keyword("mul"))),
      LRItem(NonTerminalImpl("M"), List(nonTerminalSymbol("T"), keywordSymbol("mul"), nonTerminalSymbol("N")), List(nonTerminalSymbol("T"), keywordSymbol("mul"), nonTerminalSymbol("N"))) -> Set(Terminal.eoi, Terminal(Keyword("mul"))),
      LRItem(NonTerminalImpl("N"), List(literalTokenSymbol("int", "Int")), List(literalTokenSymbol("int", "Int"))) -> Set(Terminal.eoi, Terminal(Keyword("mul")))
    ))

    val node1 = LRClosure(Map(
      LRItem(NonTerminalImpl("S"), List(nonTerminalSymbol("T")), Nil) -> Set(Terminal.eoi),
      LRItem(NonTerminalImpl("M"), List(nonTerminalSymbol("T"), keywordSymbol("mul"), nonTerminalSymbol("N")), List(keywordSymbol("mul"), nonTerminalSymbol("N"))) -> Set(Terminal.eoi, Terminal(Keyword("mul")))))

    val node2 = LRClosure(Map(
      LRItem(NonTerminalImpl("M"), List(nonTerminalSymbol("T"), keywordSymbol("mul"), nonTerminalSymbol("N")), List(nonTerminalSymbol("N"))) -> Set(Terminal.eoi, Terminal(Keyword("mul"))),
      LRItem(NonTerminalImpl("N"), List(literalTokenSymbol("int", "Int")), List(literalTokenSymbol("int", "Int"))) -> Set(Terminal.eoi, Terminal(Keyword("mul")))))


    automaton.start shouldBe node0
    automaton.edges(node0)(nonTerminalSymbol("T")) shouldBe node1
    automaton.edges(node1)(keywordSymbol("mul")) shouldBe node2
  }

  def simpleSyntax = SyntaxRule(List("Simple"), NonTerminalImpl("S"), List(
    DerivationRule(NonTerminalImpl("S"), List(nonTerminalSymbol("T"))),
    BranchRule(NonTerminalImpl("T"), List(NonTerminalImpl("M"), NonTerminalImpl("N"))),
    DerivationRule(NonTerminalImpl("M"), List(nonTerminalSymbol("T"), keywordSymbol("mul"), nonTerminalSymbol("N"))),
    DerivationRule(NonTerminalImpl("N"), List(literalTokenSymbol("int", "Int")))
  ))
}

