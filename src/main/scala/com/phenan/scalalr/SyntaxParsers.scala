package com.phenan.scalalr

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by @phenan on 2016/12/15.
  */
object SyntaxParsers extends JavaTokenParsers {
  def syntax: Parser[Syntax] = "syntax" ~> ident ~ ( "(" ~> nonTerminal <~ ")" ) ~ ( "{" ~> rule.* <~ "}" ) ^^ {
    case name ~ start ~ rules => Syntax(name, NonTerminal("#Start#"), DerivationRule(NonTerminal("#Start#"), List(start)) :: rules)
  }

  def rule: Parser[Rule] = branch | derivation

  def branch: Parser[BranchRule] = ( nonTerminal <~ "=" ) ~ rep1sep(nonTerminal, "|") <~ ";" ^^ {
    case left ~ right => BranchRule(left, right)
  }

  def derivation: Parser[DerivationRule] = ( nonTerminal <~ "=" ) ~ ( terminal | nonTerminal ).+ <~ ";" ^^ {
    case left ~ right => DerivationRule(left, right)
  }

  def nonTerminal: Parser[NonTerminal] = not(id | int) ~> ident ^^ NonTerminal

  def terminal: Parser[Terminal] = id | int | keyword

  def id: Parser[StringLiteral.type] = "id" ^^^ StringLiteral

  def int: Parser[IntLiteral.type] = "int" ^^^ IntLiteral

  def keyword: Parser[Keyword] = "\"" ~> ident <~ "\"" ^^ Keyword
}

