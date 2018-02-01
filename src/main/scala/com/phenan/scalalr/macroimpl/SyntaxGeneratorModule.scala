package com.phenan.scalalr
package macroimpl

import shared._

import shapeless._

import scala.collection._

trait SyntaxGeneratorModule {
  this: TyperModule with SyntaxInfoModule with SyntaxRuleModule with MacroModule =>

  import c.universe._

  case class NonTerminalImpl (ntType: Type)
  case class LiteralTokenImpl (literalType: Type)

  override type NonTerminal = NonTerminalImpl
  override type LiteralToken = LiteralTokenImpl
  override type SemanticAction = SemanticActionImpl

  def generateSyntax (syntaxInfoList: List[SyntaxInfo]): Syntax = SyntaxGenerator.generate(syntaxInfoList)

  private object SyntaxGenerator {
    /**
      * syntax アノテーションの情報から文法を生成する関数
      * @param syntaxInfoList syntax アノテーションの情報
      * @return 生成された文法
      */
    def generate (syntaxInfoList: List[SyntaxInfo]): Syntax = {
      val derivations = syntaxInfoList.map(generateRule)
      val inheritances = inheritanceRules(start, derivations)
      val literals = literalRules(collectNonTerminals(derivations ++ inheritances))
      Syntax(start, derivations ++ inheritances ++ literals)
    }

    /**
      * dslアノテーションの型引数から開始記号を求める関数
      * @return 開始記号
      */
    private lazy val start: NonTerminal = c.prefix.tree match {
      case Apply(Select(New(AppliedTypeTree(Ident(TypeName("dsl")), List(t))), termNames.CONSTRUCTOR), Nil) => NonTerminalImpl(typer.check(t))
      case _ => c.abort(c.prefix.tree.pos, "@dsl should take a type argument and should not take an argument")
    }

    /**
      * syntax アノテーションで指定された文法を Rule に変換する
      * @param syntaxInfo syntax アノテーションで指定された文法の情報
      * @return 対応する文法規則
      */
    private def generateRule (syntaxInfo: SyntaxInfo): Rule = {
      val left = NonTerminalImpl(typer.check(syntaxInfo.returnType))
      val operators = syntaxInfo.operators.map(_.map(Keyword))
      val operands = syntaxInfo.operandTypes.map(t => NonTerminalImpl(typer.check(t)))
      val expr = buildSyntaxExpression(operators, operands)
      Rule(left, expr, syntaxInfo.semantics)
    }

    /**
      * オペレータ(キーワード)のリストとオペランド(引数部)のリストから文法式を組み立てる
      * @param operators オペレータ(キーワード)のリスト
      * @param operands オペランド(引数部)のリスト
      * @return 文法式
      */
    private def buildSyntaxExpression (operators: List[List[Keyword]], operands: List[NonTerminal]): List[Symbol] = {
      def operatorSymbols = operators.map(_.map(k => Symbol(Terminal(k))))
      def operandSymbols = operands.map(nt => List(Symbol(nt)))
      ( Nil :: operandSymbols zip operatorSymbols ).flatMap ( pair => pair._1 ++ pair._2 )
    }

    /**
      * syntax アノテーションで指定された文法に追加する、継承関係を表現した文法規則を生成する関数
      * @param start 開始記号
      * @param derivations syntax アノテーションで指定された文法
      * @return 継承関係を表現した文法規則
      */
    private def inheritanceRules (start: NonTerminal, derivations: List[Rule]): List[Rule] = {
      val leftNonTerminals: Set[NonTerminal] = derivations.map(_.left) (breakOut)
      val rightNonTerminals: Set[NonTerminal] = collectNonTerminals(derivations)

      val inheritances: Map[NonTerminal, Set[NonTerminal]] = (leftNonTerminals ++ rightNonTerminals).map { left =>
        left -> (rightNonTerminals + start).filter(left.ntType <:< _.ntType).filterNot(left.ntType =:= _.ntType)
      } (breakOut)

      eliminateShortcut(inheritances).flatMap {
        case (left, rights) => rights.map(right => Rule(right, List(Symbol(left)), Inheritance))
      } (breakOut)
    }

    /**
      * 継承関係を表現する Map から、ショートカットとなるようなパスを削除する関数
      * 例えば、A -> B, B -> C があったとき A -> C のパスを削除する
      * @param inheritances 継承関係を表現する Map
      * @return ショートカットパスを削除したもの
      */
    private def eliminateShortcut (inheritances: Map[NonTerminal, Set[NonTerminal]]): Map[NonTerminal, Set[NonTerminal]] = {
      inheritances.map { case (left, rights) =>
        left -> rights.filterNot { right =>
          existsLongPath(left, right, inheritances)
        }
      }
    }

    private def existsLongPath (from: NonTerminal, to: NonTerminal, paths: Map[NonTerminal, Set[NonTerminal]]): Boolean = {
      paths(from).filter(_ != to).exists(via => paths.get(via).exists(_.contains(to)))
    }

    /**
      * 文法規則の右辺で利用されている全ての非終端記号の集合を求める関数
      * @param rules 文法規則
      * @return 非終端記号の集合
      */
    private def collectNonTerminals (rules: List[Rule]): Set[NonTerminal] = {
      rules.flatMap { _.right.collect { case Inl(nt) => nt } } (breakOut)
    }

    /**
      * 与えられた非終端記号に対応するリテラルを記述できる文法規則を生成する関数
      * @param nonTerminals 非終端記号の集合
      * @return 対応するリテラルの文法規則
      */
    private def literalRules (nonTerminals: Set[NonTerminal]): Set[Rule] = {
      nonTerminals.map(nt => Rule(nt, List(Symbol(Terminal(LiteralTokenImpl(nt.ntType)))), LiteralRef))
    }
  }
}
