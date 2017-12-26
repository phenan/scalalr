package com.phenan.scalalr

import collection.breakOut

/**
  * Created by @phenan on 2016/12/12.
  */
case class SyntaxRule (name: String, start: NonTerminal, rules: List[Rule]) {

  /**
    * 文法一覧
    */
  lazy val expressions: Map[NonTerminal, List[List[Symbol]]] = rules.groupBy(_.left).mapValues {
    _.flatMap {
      case BranchRule(_, rs)        => rs.map(List(_))
      case DerivationRule(_, right) => List(right)
    }
  }

  /**
    * 全ての非終端記号
    */
  lazy val nonTerminals: Set[NonTerminal] = rules.map(_.left)(breakOut)

  /**
    * 全ての終端記号
    */
  lazy val terminals: Set[Terminal] = rules.flatMap {
    case BranchRule(_, _)         => Nil
    case DerivationRule(_, right) => right.collect { case t: Terminal => t }
  } (breakOut)

  /**
    * 全てのリテラル
    */
  lazy val literals: Set[LiteralToken] = terminals.collect {
    case l: LiteralToken => l
  }

  /**
    * 与えられた文法式の先読み集合を求める関数
    * @param expr 文法式
    * @return 先読み集合
    */
  def lookupFirst (expr: List[Symbol]): Set[Terminal] = lookupFirst(expr, Set.empty, Set.empty)

  /**
    * 文法式と親の文法規則の先読み集合から、先読み集合を計算する関数
    * @param expr 文法式
    * @param lookahead 親の文法規則の先読み集合
    * @return 先読み集合
    */
  def lookupFirst (expr: List[Symbol], lookahead: Set[Terminal]): Set[Terminal] = lookupFirst(expr, lookahead, Set.empty)

  /**
    * εになりうる全ての非終端記号
    */
  private lazy val canEmpty: Set[NonTerminal] = buildCanEmpty(nonTerminals)

  /**
    * εになりうる全ての非終端記号を求める関数
    * 最初は全ての非終端記号がεになりうると仮定し、そこからεになり得ないものを除いていく。
    * それ以上除かれなくなれば終了する。単調減少のため必ず停止する。
    * @param set εになりうる非終端記号の候補
    * @return εになりうる全ての非終端記号
    */
  private def buildCanEmpty (set: Set[NonTerminal]): Set[NonTerminal] = {
    val newSet = set.filter {
      expressions(_).exists(_.forall {
        case n: NonTerminal => set.contains(n)
        case EmptyString    => true
        case _: Terminal    => false
      })
    }
    if (set == newSet) set
    else buildCanEmpty(newSet)
  }

  /**
    * first set : ある非終端記号が表現する文字列の最初の１アルファベットの集合
    */
  private lazy val firstSet: Map[NonTerminal, Set[Terminal]] = buildFirstSet(nonTerminals.map(_ -> Set.empty[Terminal])(breakOut))

  /**
    * first set を構築する関数
    * first set が変化しなくなるまで再帰的に構築する。
    * first set のサイズは単調増加するため必ず停止する。
    * 効率はそれほど良くないかもしれないが、ボトルネックになるような場所ではないと思われる。
    * @param fs 前回構築した first set
    * @return first set
    */
  private def buildFirstSet (fs: Map[NonTerminal, Set[Terminal]]): Map[NonTerminal, Set[Terminal]] = {
    val newSet = fs.map {
      case (nt, set) => nt -> expressions(nt).foldRight(set)(updateFirst(_, fs, _))
    }
    if (fs == newSet) fs
    else buildFirstSet(newSet)
  }

  /**
    * 文法式を参照して、ある非終端記号 N の first set を更新する関数
    * @param expr 文法式
    * @param fs 構築済みの first set
    * @param set 非終端記号 N の first set のアキュムレータ
    * @return 非終端記号 N の新しい first set
    */
  private def updateFirst (expr: List[Symbol], fs: Map[NonTerminal, Set[Terminal]], set: Set[Terminal]): Set[Terminal] = expr match {
    case (nt: NonTerminal) :: rest if canEmpty(nt) => updateFirst(rest, fs, set ++ fs(nt))
    case (nt: NonTerminal) :: _                    => set ++ fs(nt)
    case (t: Terminal) :: _                        => set + t
    case EmptyString :: rest                       => updateFirst(rest, fs, set)
    case Nil                                       => set
  }

  /**
    * 文法式と親の文法規則の先読み集合から、先読み集合を計算する関数
    * @param expr 文法式
    * @param lookahead 親の文法規則の先読み集合
    * @param set 先読み集合のアキュムレータ
    * @return 先読み集合
    */
  private def lookupFirst (expr: List[Symbol], lookahead: Set[Terminal], set: Set[Terminal]): Set[Terminal] = expr match {
    case (nt: NonTerminal) :: rest if canEmpty(nt) => lookupFirst(rest, lookahead, set ++ firstSet(nt))
    case (nt: NonTerminal) :: _                    => set ++ firstSet(nt)
    case (t: Terminal) :: _                        => set + t
    case EmptyString :: rest                       => lookupFirst(rest, lookahead, set)
    case Nil                                       => set ++ lookahead
  }
}

sealed trait Rule {
  def left: NonTerminal
}

case class BranchRule (left: NonTerminal, rules: List[NonTerminal]) extends Rule

case class DerivationRule (left: NonTerminal, right: List[Symbol]) extends Rule

sealed trait Symbol

trait NonTerminal extends Symbol {
  def typeName: String
}

object NonTerminal {
  def apply (name: String): NonTerminal = NonTerminalString(name)

  case class NonTerminalString (name: String) extends NonTerminal {
    override def typeName: String = name
  }
}

case object EmptyString extends Symbol

sealed trait Terminal extends Symbol

case class Keyword (kw: String) extends Terminal

case class LiteralToken (name: String, litType: String) extends Terminal

//case object StringLiteral extends Terminal

//case object IntLiteral extends Terminal

case object EndOfInput extends Terminal



