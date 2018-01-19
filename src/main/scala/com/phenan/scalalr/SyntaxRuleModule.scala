package com.phenan.scalalr

import collection.breakOut

import shapeless._

trait SyntaxRuleModule {

  import HList.ListCompat._

  type Symbol = NonTerminal :+: Terminal :+: EmptyString.type :+: CNil

  object Symbol {
    def apply (n: NonTerminal): Symbol = Coproduct[Symbol](n)
    def apply (t: Terminal): Symbol = Coproduct[Symbol](t)
    def apply (e: EmptyString.type): Symbol = Coproduct[Symbol](e)
    def epsilon: Symbol = Coproduct[Symbol](EmptyString)
  }

  type NonTerminal

  type Terminal = LiteralToken :+: Keyword :+: EndOfInput.type :+: CNil

  object Terminal {
    def apply (t: LiteralToken): Terminal = Coproduct[Terminal](t)
    def apply (k: Keyword): Terminal = Coproduct[Terminal](k)
    def apply (e: EndOfInput.type): Terminal = Coproduct[Terminal](e)
    def eoi: Terminal = Coproduct[Terminal](EndOfInput)
  }

  type LiteralToken

  case class Keyword (kw: String) {
    lazy val scalaIdent: String = kw.flatMap {
      case '!'  => "$$exclam"
      case '\"' => "$$quotedbl"
      case '#'  => "$$numbersign"
      case '%'  => "$$percent"
      case '&'  => "$$ampersand"
      case '\'' => "$$quotesingle"
      case '('  => "$$parenleft"
      case ')'  => "$$parenright"
      case '*'  => "$$asterisk"
      case '+'  => "$$plus"
      case ','  => "$$comma"
      case '-'  => "$$hyphen"
      case '.'  => "$$period"
      case '/'  => "$$slash"
      case ':'  => "$$colon"
      case ';'  => "$$semicolon"
      case '<'  => "$$less"
      case '='  => "$$equal"
      case '>'  => "$$greater"
      case '?'  => "$$question"
      case '@'  => "$$at"
      case '['  => "$$bracketleft"
      case '\\' => "$$backslash"
      case ']'  => "$$bracketright"
      case '^'  => "$$asciicircum"
      case '`'  => "$$grave"
      case '{'  => "$$braceleft"
      case '|'  => "$$bar"
      case '}'  => "$$braceright"
      case '~'  => "$$asciitilde"
      case c    => c.toString
    }
  }

  case object EndOfInput

  case object EmptyString

  /**
    * 文法規則
    */
  sealed trait Rule {
    def left: NonTerminal
  }

  /**
    * 分岐規則
    * A := B | C | D のような規則
    * @param left 左辺
    * @param rules 分岐先
    */
  case class BranchRule (left: NonTerminal, rules: List[NonTerminal]) extends Rule

  /**
    * 導出規則
    * A := x y z のような規則
    * @param left 左辺
    * @param right 文法式
    */
  case class DerivationRule (left: NonTerminal, right: List[Symbol]) extends Rule

  /**
    * 文法を表現するデータ
    * @param qualifiedName 言語の完全修飾名
    * @param start 開始記号
    * @param rules 全ての文法規則
    */
  case class SyntaxRule (qualifiedName: List[String], start: NonTerminal, rules: List[Rule]) {

    /**
      * 言語の短縮名称
      */
    lazy val name: String = qualifiedName.last

    /**
      * 文法規則一覧
      */
    lazy val expressions: Map[NonTerminal, List[List[Symbol]]] = rules.groupBy(_.left).mapValues {
      _.flatMap {
        case BranchRule(_, rs)        => rs.map(r => List(Symbol(r)))
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
      case DerivationRule(_, right) => right.collect { case Inr(Inl(t)) => t }
    } (breakOut)

    /**
      * 全てのリテラル
      */
    lazy val literals: Set[LiteralToken] = terminals.collect {
      case Inl(l) => l
    }

    /**
      * 全てのキーワード (オペレータ)
      */
    lazy val keywords: Set[Keyword] = terminals.collect {
      case Inr(Inl(k)) => k
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
        expressions(_).exists {
          _.forall {
            case Inl(n)      => set.contains(n)
            case Inr(Inl(_)) => false
            case Inr(Inr(_)) => true
          }
        }
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
      case Inl(nt) :: rest if canEmpty(nt) => updateFirst(rest, fs, set ++ fs(nt))
      case Inl(nt) :: _                    => set ++ fs(nt)
      case Inr(Inl(t)) :: _                => set + t
      case Inr(Inr(_)) :: rest             => updateFirst(rest, fs, set)
      case Nil                             => set
    }

    /**
      * 文法式と親の文法規則の先読み集合から、先読み集合を計算する関数
      * @param expr 文法式
      * @param lookahead 親の文法規則の先読み集合
      * @param set 先読み集合のアキュムレータ
      * @return 先読み集合
      */
    private def lookupFirst (expr: List[Symbol], lookahead: Set[Terminal], set: Set[Terminal]): Set[Terminal] = expr match {
      case Inl(nt) :: rest if canEmpty(nt) => lookupFirst(rest, lookahead, set ++ firstSet(nt))
      case Inl(nt) :: _                    => set ++ firstSet(nt)
      case Inr(Inl(t)) :: _                => set + t
      case Inr(Inr(_)) :: rest             => lookupFirst(rest, lookahead, set)
      case Nil                             => set ++ lookahead
    }
  }
}
