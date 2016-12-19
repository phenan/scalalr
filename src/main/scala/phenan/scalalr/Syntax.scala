package phenan.scalalr

import collection.breakOut

/**
  * Created by @phenan on 2016/12/12.
  */
case class Syntax (name: String, start: NonTerminal, rules: List[Rule]) {
  lazy val expressions: Map[NonTerminal, List[List[Symbol]]] = rules.groupBy(_.left).mapValues {
    _.flatMap {
      case BranchRule(_, rs)        => rs.map(List(_))
      case DerivationRule(_, right) => List(right)
    }
  }

  lazy val nonTerminals: Set[NonTerminal] = rules.map(_.left)(breakOut)
  lazy val terminals: Set[Terminal] = rules.flatMap {
    case BranchRule(_, _)         => Nil
    case DerivationRule(_, right) => right.collect { case t: Terminal => t }
  } (breakOut)

  def first (expr: List[Symbol]): Set[Terminal] = first(expr, firstSet, Set.empty)

  def first (expr: List[Symbol], set: Set[Terminal]): Set[Terminal] = first(expr, firstSet, set)

  private def first (expr: List[Symbol], firstSet: Map[NonTerminal, Set[Terminal]], set: Set[Terminal]): Set[Terminal] = expr match {
    case (nt: NonTerminal) :: rest if firstSet(nt).contains(EmptyString) => first(rest, firstSet, set ++ firstSet(nt))
    case (nt: NonTerminal) :: _ => set ++ firstSet(nt)
    case EmptyString :: rest    => first(rest, firstSet, set)
    case (t: Terminal) :: _     => Set(t)
    case Nil                    => set
  }

  private lazy val firstSet: Map[NonTerminal, Set[Terminal]] = buildFirstSet(nonTerminals.map(_ -> Set.empty[Terminal])(breakOut))

  private def buildFirstSet (fs: Map[NonTerminal, Set[Terminal]]): Map[NonTerminal, Set[Terminal]] = {
    val newSet = fs ++ growFirstSet(fs)
    if (fs == newSet) fs
    else buildFirstSet(newSet)
  }

  private def growFirstSet (fs: Map[NonTerminal, Set[Terminal]]): Map[NonTerminal, Set[Terminal]] = fs.map {
    case (nt, set) => nt -> expressions(nt).foldRight(set)(first(_, fs, _))
  }
}

sealed trait Rule {
  def left: NonTerminal
}

case class BranchRule (left: NonTerminal, rules: List[NonTerminal]) extends Rule

case class DerivationRule (left: NonTerminal, right: List[Symbol]) extends Rule

sealed trait Symbol

case class NonTerminal (name: String) extends Symbol

sealed trait Terminal extends Symbol

case class Keyword (kw: String) extends Terminal

case object StringLiteral extends Terminal

case object IntLiteral extends Terminal

case object EndOfInput extends Terminal

case object EmptyString extends Terminal

