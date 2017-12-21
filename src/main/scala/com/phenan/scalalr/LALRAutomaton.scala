package com.phenan.scalalr

import scala.collection.breakOut

/**
  * Created by @phenan on 2016/12/12.
  */
case class LALRAutomaton (syntax: Syntax) {

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
    _.collect { case (t: Terminal, n) => (t, n) }
  }

  /**
    * reduce: 非終端記号の導出による遷移
    * 遷移元 LR closure -> (reduce が起こりうる終端記号 -> (導出される非終端記号, 導出元の文法式))
    */
  lazy val reduce: Map[LRClosure, Map[Terminal, (NonTerminal, List[Symbol])]] = nodes.flatMap { closure =>
    closure.items.collect { case (LRItem(nt, expr, Nil), lookahead) =>
      closure -> lookahead.map(_ -> (nt -> expr)).toMap
    }
  } (breakOut)

  /**
    * goto: 非終端記号による遷移
    * 遷移元 LR closure -> (非終端記号 -> 遷移先 LR closure)
    */
  lazy val goTo: Map[LRClosure, Map[NonTerminal, LRClosure]] = edges.mapValues {
    _.collect { case (nt: NonTerminal, n) => (nt, n) }
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

case class LR1Automaton (syntax: Syntax) {
  /**
    * 最初の LR closure
    * LR オートマトンの開始地点となる。
    */
  lazy val start: LRClosure = {
    growLRClosure(LRClosure(syntax.expressions(syntax.start).map(e => LRItem(syntax.start, e, e) -> Set[Terminal](EndOfInput)).toMap))
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
    val newClosure = closure ++ closure.items.collect { case (LRItem(_, _, (n: NonTerminal) :: rest), lookahead) =>
      syntax.expressions(n).map(e => LRItem(n, e, e) -> syntax.lookupFirst(rest, lookahead))
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
    case (LRItem(nt, expr, symbol :: rest), lookahead) => (symbol, LRItem(nt, expr, rest), lookahead)
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

case class LRItem (nt: NonTerminal, expr: List[Symbol], rest: List[Symbol])

/*
case class LALRAutomaton (syntax: Syntax, start: LRNode, edges: Map[LRNode, Map[Symbol, LRNode]], closures: Map[LRNode, LRClosure]) {
  lazy val shift: Map[LRNode, Map[Terminal, LRNode]] = edges.mapValues {
    _.collect { case (t: Terminal, n) => (t, n) }
  }

  lazy val reduce: Map[LRNode, Map[Terminal, (NonTerminal, List[Symbol])]] = nodes.flatMap { node =>
    node.items.collect { case item @ LRItem(nt, expr, Nil) =>
      node -> closures(node).lookahead(item).map(_ -> (nt -> expr)).toMap
    }
  } (breakOut)

  lazy val goTo: Map[LRNode, Map[NonTerminal, LRNode]] = edges.mapValues {
    _.collect { case (nt: NonTerminal, n) => (nt, n) }
  }

  lazy val nodes: Set[LRNode] = closures.keySet

  lazy val reverseEdges: Map[LRNode, Set[LRNode]] = state.map { case (to, symbol) =>
    to -> edges.collect { case (from, map) if map.get(symbol).contains(to) => from }.toSet
  }

  lazy val state: Map[LRNode, Symbol] = edges.values.flatMap(_.map(_.swap))(breakOut)
}

object LALRAutomaton {
  def apply (syntax: Syntax): LALRAutomaton = {
    val lr1 = LR1Automaton(syntax)
    val es = lr1.edges.groupBy(_._1.node)
    LALRAutomaton(lr1.syntax, lr1.start.node, es.mapValues(_.values.map(_.mapValues(_.node)).reduce(_ ++ _)), es.mapValues(_.keys.reduce(_.union(_))))
  }
}

case class LR1Automaton (syntax: Syntax, start: LRClosure, edges: Map[LRClosure, Map[Symbol, LRClosure]])

object LR1Automaton {
  def apply (syntax: Syntax): LR1Automaton = {
    val start = LRClosure.seed(syntax)
    grow(syntax, start, Map.empty, List(start))
  }

  private def grow (syntax: Syntax, start: LRClosure, edges: Map[LRClosure, Map[Symbol, LRClosure]], closures: List[LRClosure]): LR1Automaton = closures match {
    case closure :: rest if edges.contains(closure) => grow(syntax, start, edges, rest)
    case closure :: rest => grow(syntax, start, edges + (closure -> closure.edges), rest ++ closure.edges.values)
    case Nil => LR1Automaton(syntax, start, edges)
  }
}

/**
  * LR closure : 先読み集合付きの LR item 集合
  * @param syntax 文法全体
  * @param lookahead LR item と 先読み集合の組み合わせ
  */
case class LRClosure (syntax: Syntax, lookahead: Map[LRItem, Set[Terminal]]) {
  lazy val node: LRNode = LRNode(lookahead.keySet)

  lazy val edges: Map[Symbol, LRClosure] = transitions(syntax).groupBy(_._1).mapValues { es =>
    LRClosure.build(syntax, es.map { case (_, item, la) => item -> la } (breakOut))
  }

  def union (that: LRClosure): LRClosure = LRClosure(syntax, LRClosure.union(lookahead, that.lookahead))

  private def transitions (syntax: Syntax): Iterable[(Symbol, LRItem, Set[Terminal])] = lookahead.collect {
    case (LRItem(nt, expr, symbol :: rest), la) => (symbol, LRItem(nt, expr, rest), la)
  }
}

object LRClosure {
  def seed (syntax: Syntax): LRClosure = {
    build(syntax, syntax.expressions(syntax.start).map(e => LRItem(syntax.start, e, e) -> Set[Terminal](EndOfInput))(breakOut))
  }

  def build (syntax: Syntax, map: Map[LRItem, Set[Terminal]]): LRClosure = {
    val newMap = grow(syntax, map).fold(map)(union)
    if (map == newMap) LRClosure(syntax, map)
    else build(syntax, newMap)
  }

  private def grow (syntax: Syntax, map: Map[LRItem, Set[Terminal]]): Iterable[Map[LRItem, Set[Terminal]]] = map.map {
    case (item, set) => grow(syntax, item, set)
  }

  private def grow (syntax: Syntax, item: LRItem, set: Set[Terminal]): Map[LRItem, Set[Terminal]] = item match {
    case LRItem(_, _, (n: NonTerminal) :: rest) => syntax.expressions(n).map(e => LRItem(n, e, e) -> syntax.first(rest, set))(breakOut)
    case _                                      => Map.empty
  }

  private def union (a: Map[LRItem, Set[Terminal]], b: Map[LRItem, Set[Terminal]]): Map[LRItem, Set[Terminal]] = {
    a.foldLeft(b) { case (map, (t, set)) => map + (t -> (map.getOrElse(t, Set.empty) ++ set)) }
  }
}

case class LRNode (items: Set[LRItem])


case class LRItem (nt: NonTerminal, expr: List[Symbol], rest: List[Symbol])
*/

