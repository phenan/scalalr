package com.phenan.scalalr

import cli._

import org.scalatest._

/**
  * Created by @phenan on 2016/12/12.
  */
class LALRAutomatonTest extends FunSuite with Matchers {
  import CLIApplication._

  test ("LALR automaton") {
    val automaton = LALRAutomaton(simpleSyntax)

    val node0 = LRClosure(Map(
      LRItem(derivation_S_T, List(nonTerminalSymbol("T"))) -> Set(Terminal.eoi),
      LRItem(branch_T_M, List(nonTerminalSymbol("M"))) -> Set(Terminal.eoi, Terminal(Keyword("mul"))),
      LRItem(branch_T_N, List(nonTerminalSymbol("N"))) -> Set(Terminal.eoi, Terminal(Keyword("mul"))),
      LRItem(derivation_M_mul, List(nonTerminalSymbol("T"), keywordSymbol("mul"), nonTerminalSymbol("N"))) -> Set(Terminal.eoi, Terminal(Keyword("mul"))),
      LRItem(derivation_N_int, List(literalTokenSymbol("int", "Int"))) -> Set(Terminal.eoi, Terminal(Keyword("mul")))
    ))

    val node1 = LRClosure(Map(
      LRItem(derivation_S_T, Nil) -> Set(Terminal.eoi),
      LRItem(derivation_M_mul, List(keywordSymbol("mul"), nonTerminalSymbol("N"))) -> Set(Terminal.eoi, Terminal(Keyword("mul")))))

    val node2 = LRClosure(Map(
      LRItem(derivation_M_mul, List(nonTerminalSymbol("N"))) -> Set(Terminal.eoi, Terminal(Keyword("mul"))),
      LRItem(derivation_N_int, List(literalTokenSymbol("int", "Int"))) -> Set(Terminal.eoi, Terminal(Keyword("mul")))))


    automaton.start shouldBe node0
    automaton.edges(node0)(nonTerminalSymbol("T")) shouldBe node1
    automaton.edges(node1)(keywordSymbol("mul")) shouldBe node2
  }

  def simpleSyntax = SyntaxRule(NonTerminalImpl("S"), List(derivation_S_T, branch_T_M, branch_T_N, derivation_M_mul, derivation_N_int))

  private lazy val derivation_S_T: Rule = Rule(NonTerminalImpl("S"), List(nonTerminalSymbol("T")), Derivation)
  private lazy val branch_T_M: Rule = Rule(NonTerminalImpl("T"), List(nonTerminalSymbol("M")), Branch)
  private lazy val branch_T_N: Rule = Rule(NonTerminalImpl("T"), List(nonTerminalSymbol("N")), Branch)
  private lazy val derivation_M_mul: Rule = Rule(NonTerminalImpl("M"), List(nonTerminalSymbol("T"), keywordSymbol("mul"), nonTerminalSymbol("N")), Derivation)
  private lazy val derivation_N_int: Rule = Rule(NonTerminalImpl("N"), List(literalTokenSymbol("int", "Int")), Derivation)
}

