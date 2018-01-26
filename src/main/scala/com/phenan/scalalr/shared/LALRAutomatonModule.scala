package com.phenan.scalalr.shared

import shapeless._

import collection.breakOut

trait LALRAutomatonModule {
  self: SyntaxRuleModule =>

  import HList.ListCompat._

  case class LALRAutomaton (syntax: SyntaxRule) {

    lazy val start: LRClosure = mappingLR1toLALR(lr1.start)

    /**
      * 全てのエッジの集合
      */
    lazy val edges: Map[LRClosure, Map[Symbol, LRClosure]] = lr1.edges.toSet.map { (pair: (LRClosure, Map[Symbol, LRClosure])) =>
      mappingLR1toLALR(pair._1) -> pair._2.mapValues(mappingLR1toLALR)
    }.groupBy(_._1).mapValues(_.map(_._2).reduce(unionEdges))

    /**
      * nodes: 全ての LR closure
      */
    lazy val nodes: Set[LRClosure] = edges.keySet

    /**
      * shift: 終端記号による遷移
      * 遷移元 LR closure -> (終端記号 -> 遷移先 LR closure)
      */
    lazy val shift: Map[LRClosure, Map[Terminal, LRClosure]] = edges.mapValues {
      _.collect { case (Inr(Inl(t)), n) => (t, n) }
    }

    /**
      * reduce: 非終端記号の導出による遷移
      * 遷移元 LR closure -> (文法式, 先読み記号の集合)
      */
    lazy val reduce: Map[LRClosure, (Rule, Set[Terminal])] = nodes.flatMap { closure =>
      closure.items.collect { case (LRItem(rule, Nil), lookahead) =>
        (closure, (rule, lookahead))
      }
    } (breakOut)

    /**
      * goto: 非終端記号による遷移
      * 遷移元 LR closure -> (非終端記号 -> 遷移先 LR closure)
      */
    lazy val goTo: Map[LRClosure, Map[NonTerminal, LRClosure]] = edges.mapValues {
      _.collect { case (Inl(nt), n) => (nt, n) }
    }

    /**
      * accept: 終了状態となるような LR closure
      */
    lazy val accept: Set[LRClosure] = nodes.filter {
      _.items.exists { case (item, lookahead) =>
        item.rule.left == syntax.start && item.rest.isEmpty && lookahead.contains(Terminal.eoi)
      }
    }

    /**
      * state: LR closure の持つ状態
      * ある LR closure への遷移は全て同じ記号によるものであると仮定し、その対応関係を表現する
      */
    lazy val state: Map[LRClosure, Symbol] = {
      edges.values.flatten.groupBy(_._2).mapValues(pair => ensureEqualAll(pair.map(_._1)))
    }

    /**
      * エッジを逆にたどる Map
      * Symbol の情報は state の方に入っているため、ここでは省略している
      */
    lazy val reverseEdges: Map[LRClosure, Set[LRClosure]] = state.map { case (to, symbol) =>
      to -> edges.collect { case (from, map) if map.get(symbol).contains(to) => from }.toSet
    }

    /**
      * LR(1) automaton における LR closure から LALR(1) automaton における LR closure へのマッピング
      * LALR(1) automaton では LR(1) automaton における LR closure を併合して LR closure の数を減らす。
      */
    private lazy val mappingLR1toLALR: Map[LRClosure, LRClosure] = lr1.edges.keySet.groupBy(_.items.keySet).values.flatMap { set =>
      val closure = set.reduce(_ ++ _)
      set.map(_ -> closure)
    }.toMap

    /**
      * エッジの集合を合成する関数
      * @param e1 エッジの集合その１
      * @param e2 エッジの集合その2
      * @return 合成したエッジの集合
      */
    private def unionEdges (e1: Map[Symbol, LRClosure], e2: Map[Symbol, LRClosure]): Map[Symbol, LRClosure] = {
      e1.foldLeft(e2) { case (e, (s, c)) =>
        if (e.contains(s) && e(s) == c) e
        else if (! e.contains(s)) e + (s -> c)
        else throw new RuntimeException("broken LALR automaton")
      }
    }

    private def ensureEqualAll (s: Iterable[Symbol]): Symbol = {
      require(s.nonEmpty)
      if (s.tail.forall(_ == s.head)) s.head
      else throw new RuntimeException(s"cannot unify the state of LR closure: $s")
    }

    private lazy val lr1 = LR1Automaton(syntax)
  }

  case class LR1Automaton (syntax: SyntaxRule) {
    /**
      * 最初の LR closure
      * LR オートマトンの開始地点となる。
      */
    lazy val start: LRClosure = {
      growLRClosure(LRClosure(syntax.expressions(syntax.start).map(r => LRItem(r, r.right) -> Set(Terminal.eoi)).toMap))
    }

    /**
      * 全てのエッジを表現する Map
      */
    lazy val edges: Map[LRClosure, Map[Symbol, LRClosure]] = growEdges(List(start), Map.empty)

    /**
      * LR closure を成長させる関数
      * LR closure は A -> x・B y [w] を含むとき、B -> ・z [first(yw)] も含む。
      * この更新操作を収束するまで繰り返し行う。
      * @param closure 更新対象となる LR closure
      * @return 完成した LR closure
      */
    private def growLRClosure (closure: LRClosure): LRClosure = {
      val newClosure = closure ++ closure.items.collect { case (LRItem(_, Inl(n) :: rest), lookahead) =>
        syntax.expressions(n).map(r => LRItem(r, r.right) -> syntax.lookupFirst(rest, lookahead))
      }.flatten
      if (closure == newClosure) closure
      else growLRClosure(newClosure)
    }

    /**
      * 与えられた LR closure から張られるエッジとそこから推移的に張られるエッジを全て求める関数
      * @param closures まだ調べていない LR closure
      * @param edges エッジのアキュムレータ
      * @return 全てのエッジを表す Map
      */
    private def growEdges (closures: List[LRClosure], edges: Map[LRClosure, Map[Symbol, LRClosure]]): Map[LRClosure, Map[Symbol, LRClosure]] = closures match {
      case closure :: rest if edges.contains(closure) => growEdges(rest, edges)
      case closure :: rest =>
        val es = closureEdges(closure)
        growEdges(rest ++ es.values, edges + (closure -> es))
      case Nil => edges
    }

    /**
      * 与えられた LR closure から張られる全てのエッジを返す関数
      * @param closure LR closure
      * @return どのシンボルによってどの LR closure に遷移するかを示す Map
      */
    private def closureEdges (closure: LRClosure): Map[Symbol, LRClosure] = transitions(closure).groupBy(_._1).mapValues {
      es => growLRClosure(LRClosure(es.map { case (_, item, lookahead) => item -> lookahead }.toMap))
    }

    /**
      * LR closure の各アイテムに関して、全ての遷移をリストアップする関数
      * @param closure LR closure
      * @return 遷移のために必要なシンボル, 遷移先の LR item, 遷移後の先読み集合
      */
    private def transitions (closure: LRClosure): Iterable[(Symbol, LRItem, Set[Terminal])] = closure.items.collect {
      case (LRItem(rule, symbol :: rest), lookahead) => (symbol, LRItem(rule, rest), lookahead)
    }
  }

  case class LRClosure (items: Map[LRItem, Set[Terminal]]) {
    def ++ (that: LRClosure): LRClosure = this ++ that.items
    def ++ (newItems: Iterable[(LRItem, Set[Terminal])]): LRClosure = LRClosure {
      newItems.foldLeft(items) { case (map, (item, lookahead)) =>
        map + (item -> (map.getOrElse(item, Set.empty) ++ lookahead))
      }
    }
  }

  case class LRItem (rule: Rule, rest: List[Symbol])

}
