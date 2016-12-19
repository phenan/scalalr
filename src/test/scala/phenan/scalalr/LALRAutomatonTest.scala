package phenan.scalalr

import org.scalatest._

/**
  * Created by @phenan on 2016/12/12.
  */
class LALRAutomatonTest extends FunSuite with Matchers {
  test ("LRClosure seed") {
    LRClosure.seed(simpleSyntax) shouldBe LRClosure(simpleSyntax, Map(
      LRItem(NonTerminal("S"), List(NonTerminal("T")), List(NonTerminal("T"))) -> Set(EndOfInput),
      LRItem(NonTerminal("T"), List(NonTerminal("M")), List(NonTerminal("M"))) -> Set(EndOfInput, Keyword("mul")),
      LRItem(NonTerminal("T"), List(NonTerminal("N")), List(NonTerminal("N"))) -> Set(EndOfInput, Keyword("mul")),
      LRItem(NonTerminal("M"), List(NonTerminal("T"), Keyword("mul"), NonTerminal("N")), List(NonTerminal("T"), Keyword("mul"), NonTerminal("N"))) -> Set(EndOfInput, Keyword("mul")),
      LRItem(NonTerminal("N"), List(IntLiteral), List(IntLiteral)) -> Set(EndOfInput, Keyword("mul"))
    ))
  }

  test ("LALR automaton") {
    val automaton = LALRAutomaton(simpleSyntax)

    val node0 = LRNode(Set(
      LRItem(NonTerminal("S"), List(NonTerminal("T")), List(NonTerminal("T"))),
      LRItem(NonTerminal("T"), List(NonTerminal("M")), List(NonTerminal("M"))),
      LRItem(NonTerminal("T"), List(NonTerminal("N")), List(NonTerminal("N"))),
      LRItem(NonTerminal("M"), List(NonTerminal("T"), Keyword("mul"), NonTerminal("N")), List(NonTerminal("T"), Keyword("mul"), NonTerminal("N"))),
      LRItem(NonTerminal("N"), List(IntLiteral), List(IntLiteral))))

    val node1 = LRNode(Set(
      LRItem(NonTerminal("S"), List(NonTerminal("T")), Nil),
      LRItem(NonTerminal("M"), List(NonTerminal("T"), Keyword("mul"), NonTerminal("N")), List(Keyword("mul"), NonTerminal("N")))))

    val node2 = LRNode(Set(
      LRItem(NonTerminal("M"), List(NonTerminal("T"), Keyword("mul"), NonTerminal("N")), List(NonTerminal("N"))),
      LRItem(NonTerminal("N"), List(IntLiteral), List(IntLiteral))))

    automaton.edges(node0)(NonTerminal("T")) shouldBe node1
    automaton.edges(node1)(Keyword("mul")) shouldBe node2
  }

  def simpleSyntax = Syntax("Simple", NonTerminal("S"), List(
    DerivationRule(NonTerminal("S"), List(NonTerminal("T"))),
    BranchRule(NonTerminal("T"), List(NonTerminal("M"), NonTerminal("N"))),
    DerivationRule(NonTerminal("M"), List(NonTerminal("T"), Keyword("mul"), NonTerminal("N"))),
    DerivationRule(NonTerminal("N"), List(IntLiteral))
  ))
}

