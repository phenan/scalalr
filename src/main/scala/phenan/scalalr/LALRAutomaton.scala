package phenan.scalalr

import scala.collection.breakOut

/**
  * Created by @phenan on 2016/12/12.
  */
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
  def apply (syntax: Syntax): LALRAutomaton = apply(LR1Automaton(syntax))

  def apply (lr1: LR1Automaton): LALRAutomaton = {
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

