package com.phenan.scalalr.shared

import shapeless._

import collection.breakOut

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
    lazy val scalaIdent: String = translateKeyword(kw)
  }

  case object EndOfInput

  case object EmptyString

  type SemanticAction

  /**
    * 文法規則
    */
  case class Rule (left: NonTerminal, right: List[Symbol], action: SemanticAction)

  /**
    * 文法を表現するデータ
    * @param start 開始記号
    * @param rules 全ての文法規則
    */
  case class Syntax (start: NonTerminal, rules: List[Rule]) {

    /**
      * 文法規則一覧
      */
    lazy val expressions: Map[NonTerminal, List[Rule]] = rules.groupBy(_.left)

    /**
      * 全ての非終端記号
      */
    lazy val nonTerminals: Set[NonTerminal] = rules.map(_.left)(breakOut)

    /**
      * 全ての終端記号
      */
    lazy val terminals: Set[Terminal] = rules.flatMap { rule =>
      rule.right.collect { case Inr(Inl(t)) => t }
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
          _.right.forall {
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
        case (nt, set) => nt -> expressions(nt).map(_.right).foldRight(set)(updateFirst(_, fs, _))
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

  def projectRule (r: Rule): String = {
    s"${r.left.toString} := ${r.right.map(projectSymbol).mkString(" ")}"
  }

  def projectSymbol (s: Symbol): String = s match {
    case Inl(nt)     => nt.toString
    case Inr(Inl(t)) => projectTerminal(t)
    case Inr(Inr(_)) => "ε"
  }

  def projectTerminal (terminal: Terminal): String = terminal match {
    case Inl(lit)    => lit.toString
    case Inr(Inl(k)) => k.kw
    case Inr(Inr(_)) => "$"
  }

  private def translateKeyword (kw: String): String = kw match {
    case "abstract"  => "$$abstract"
    case "case"      => "$$case"
    case "catch"     => "$$catch"
    case "class"     => "$$class"
    case "def"       => "$$def"
    case "do"        => "$$do"
    case "else"      => "$$else"
    case "extends"   => "$$extends"
    case "false"     => "$$false"
    case "final"     => "$$final"
    case "finally"   => "$$finally"
    case "for"       => "$$for"
    case "forSome"   => "$$forSome"
    case "if"        => "$$if"
    case "implicit"  => "$$implicit"
    case "import"    => "$$import"
    case "lazy"      => "$$lazy"
    case "match"     => "$$match"
    case "new"       => "$$new"
    case "null"      => "$$null"
    case "object"    => "$$object"
    case "override"  => "$$override"
    case "package"   => "$$package"
    case "private"   => "$$private"
    //case "protected" => "$$ protected" // なぜか IntelliJ では protected だけバグる
    case "return"    => "$$return"
    case "sealed"    => "$$sealed"
    case "super"     => "$$super"
    case "this"      => "$$this"
    case "throw"     => "$$throw"
    case "trait"     => "$$trait"
    case "true"      => "$$true"
    case "try"       => "$$try"
    case "type"      => "$$type"
    case "val"       => "$$val"
    case "var"       => "$$var"
    case "while"     => "$$while"
    case "with"      => "$$with"
    case "yield"     => "$$yield"
    case _ => kw.flatMap {
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
      case '`'   => "$$grave"
      case '{'  => "$$braceleft"
      case '|'  => "$$bar"
      case '}'  => "$$braceright"
      case '~'  => "$$asciitilde"
      case c    => c.toString
    }
  }
}
