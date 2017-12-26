package com.phenan.scalalr

import collection.breakOut

/**
  * Created by @phenan on 2016/12/19.
  */
case class CodeGenerator (automaton: LALRAutomaton) {
  def parserProgram: String = {
    "import scala.language.implicitConversions\n" +
      s"object ${automaton.syntax.name} ${dslMembers.mkString("{\n  ", "\n  ", "\n}")}"
  }

  private def dslMembers: List[String] = {
    separator ++ nonTerminalClassDefs ++
      separator ++ shiftReduceBaseClassDefs ++
      separator ++ nodeClassDefs ++
      separator ++ literalTokenTypeDefs ++ keywordObjectTypeDefs ++
      separator ++ literalTokenMethodDefs ++ keywordMethodDefs ++
      separator ++ transitionImplicitDefs ++ transition_keywords ++
      separator ++ literalTokenImplicitDefs ++
      separator ++ shiftImplicitDefs ++ reduceImplicitDefs
  }

  private def separator: List[String] = List("", "//////////////////////////////////////////////////////////////////////////////////////////////////////////", "")

  private def nonTerminalClassDefs: List[String] = automaton.syntax.nonTerminals.filter(_ != automaton.syntax.start).map {
    case nt if traitNonTerminals.contains(nt) => superNonTerminal(nt) match {
      case Some(sup) => s"sealed trait ${symbolTypeNames(nt)} extends ${symbolTypeNames(sup)}"
      case None      => s"sealed trait ${symbolTypeNames(nt)}"
    }
    case nt => superNonTerminal(nt) match {
      case Some(sup) => s"case class ${symbolTypeNames(nt)} ${constructorArgumentListString(nt)} extends ${symbolTypeNames(sup)}"
      case None      => s"case class ${symbolTypeNames(nt)} ${constructorArgumentListString(nt)}"
    }
  } (breakOut)

  private def shiftReduceBaseClassDefs: List[String] = List (
    "case class Shift [T, N1, N2] (shift: (N1, T) => N2)",
    "case class Reduce [T, N1, N2] (reduce: N1 => N2)",
    "case class Accept [NX, R] (accept: NX => R)",
    "case class Transition [T, N1, N2] (transit: (N1, T) => N2)"
  )

  private def nodeClassDefs: List[String] = automaton.nodes.map { node =>
    if (automaton.start == node) s"case object Node${nodeIds(node)}"
    else automaton.state(node) match {
      case nt: NonTerminal    => s"case class Node${nodeIds(node)} [+NX] (prev: NX, value: ${nt.typeName})"
      case LiteralToken(_, t) => s"case class Node${nodeIds(node)} [+NX] (prev: NX, value: $t)"
      case _                  => s"case class Node${nodeIds(node)} [+NX] (prev: NX)"
    }
  } (breakOut)

  private def literalTokenTypeDefs: List[String] = {
    automaton.syntax.literals.toList.map { literal =>
      edgesFromStart.get(literal) match {
        case Some(node) => s"type ${symbolTypeNames(literal)} = Node${nodeIds(node)}[Node${nodeIds(automaton.start)}.type]"
        case None       => s"case class ${symbolTypeNames(literal)} (value: ${literal.litType})"
      }
    } :+ "case object EoI"
  }

  private def keywordObjectTypeDefs: List[String] = automaton.syntax.terminals.collect { case k @ Keyword(kw) =>
    edgesFromStart.get(k) match {
      case Some(node) => s"lazy val ${kw.capitalize} = Node${nodeIds(node)}(Node${nodeIds(automaton.start)})"
      case None       => s"case object ${kw.capitalize}"
    }
  } (breakOut)

  private def literalTokenMethodDefs: List[String] = {
    automaton.syntax.literals.toList.map { literal =>
      s"def ${literal.name} (value: ${literal.litType}): ${symbolTypeNames(literal)} = ${constructLiteral(literal, "value")}"
    } :+ "def $$ : EoI.type = EoI"
  }

  private def keywordMethodDefs: List[String] = automaton.syntax.terminals.collect {
    case Keyword(kw) => s"def $kw : ${kw.capitalize}.type = ${kw.capitalize}"
  } (breakOut)

  private def transitionImplicitDefs: List[String] = List (
    "implicit def shift_transition [T, N1, N2] (implicit shift: Shift[T, N1, N2]): Transition[T, N1, N2] = Transition { shift.shift }",
    "implicit def reduce_transition [T, N1, N2, N3] (implicit reduce: Reduce[T, N1, N2], transition: Transition[T, N2, N3]): Transition[T, N1, N3]" +
      " = Transition { (state, terminal) => transition.transit(reduce.reduce(state), terminal) }",
    "implicit def accept_transition [NX, R] (implicit accept: Accept[NX, R]): Transition[EoI.type, NX, R] = Transition { (n, _) => accept.accept(n) }",
    "implicit def accept_ast[NX, R] (node: NX)(implicit transition: Transition[EoI.type, NX, R]): R = transition.transit(node, EoI)"
  )

  private def transition_keywords: List[String] = automaton.syntax.terminals.collect { case k @ Keyword(kw) =>
    val double_transitions: List[String] = automaton.syntax.terminals.map { t =>
      s"def $kw [N3] (value: ${symbolTypeNames(t)})(implicit transition2: Transition[${symbolTypeNames(t)}, N2, N3]): N3 = transition2.transit(transition1.transit(node, ${kw.capitalize}), value)"
    } (breakOut)
    s"implicit class transition_$kw [N1, N2] (node: N1) (implicit transition1: Transition[${symbolTypeNames(k)}, N1, N2]) {\n" +
      s"  ${double_transitions.mkString("\n  ")}\n" +
      s"  def $kw [R] (eoi: EoI.type) (implicit transition2: Transition[EoI.type, N2, R]): R = transition2.transit(transition1.transit(node, ${kw.capitalize}), EoI)\n" +
      s"}"
  } (breakOut)

  private def literalTokenImplicitDefs: List[String] = automaton.syntax.literals.toList.map { literal =>
    s"implicit class transition_${literal.name} [N1, N2] (node: N1) (implicit transition: Transition[${symbolTypeNames(literal)}, N1, N2]) { def ${literal.name} (value: ${literal.litType}): N2 = transition.transit(node, ${constructLiteral(literal, "value")}) }"
  }

  private def shiftImplicitDefs: Iterable[String] = for {
    (from, map) <- automaton.shift
    (term, to)  <- map
  } yield {
    val methodName = s"node${nodeIds(from)}_shift_node${nodeIds(to)}"
    val typeInfo =
      if (automaton.start == from) s": Shift[${symbolTypeNames(term)}, Node${nodeIds(from)}.type, Node${nodeIds(to)}[Node${nodeIds(from)}.type]]"
      else s"[NX] : Shift[${symbolTypeNames(term)}, Node${nodeIds(from)}[NX], Node${nodeIds(to)}[Node${nodeIds(from)}[NX]]]"

    val body = automaton.state(to) match {
      case LiteralToken(_, _) => s"Shift((s, t) => Node${nodeIds(to)}(s, t.value))"
      case _                  => s"Shift((s, _) => Node${nodeIds(to)}(s))"
    }

    s"implicit def $methodName $typeInfo = $body"
  }

  private def reduceImplicitDefs: Iterable[String] = for {
    (from, map) <- reduceAndGoTo
    (term, set) <- map
    (dest, path, nt)  <- set
  } yield dest match {
    case Some(to) => makeImplicitReduceString(from, to, term, path, nt)
    case None =>
      s"implicit def node${nodeIds(from)}_accept[NX]: Accept[Node${nodeIds(from)}[NX], ${symbolTypeNames(automaton.state(from))}] = Accept(s => s.value)"
  }

  private def makeImplicitReduceString (from: LRClosure, to: LRClosure, term: Terminal, path: List[LRClosure], nt: NonTerminal): String = {
    val baseType   = if (automaton.start == path.head) s"Node${nodeIds(path.head)}.type" else s"Node${nodeIds(path.head)}[NX]"
    val fromState  = path.tail.foldLeft(baseType) { (arg, node) => s"Node${nodeIds(node)}[$arg]" }

    val methodName = s"node${nodeIds(from)}_reduce_node${nodeIds(to)}_${terminalNames(term)}_${path.init.map(nodeIds).mkString("_")}"
    val typeInfo   =
      if (automaton.start == path.head) s": Reduce[${symbolTypeNames(term)}, $fromState, Node${nodeIds(to)}[$baseType]]"
      else s"[NX] : Reduce[${symbolTypeNames(term)}, $fromState, Node${nodeIds(to)}[$baseType]]"

    val baseState  = path.init.foldRight("s") { (_, r) => s"$r.prev" }

    val toState =
      if (traitNonTerminals.contains(nt)) "s.value"
      else symbolTypeNames(nt) + path.tail.foldRight[(String, List[String])] (("s", Nil)) { case (node, (cur, as)) =>
        automaton.state(node) match {
          case _: NonTerminal | _: LiteralToken => (s"$cur.prev", s"$cur.value" :: as)
          case _                                => (s"$cur.prev", as)
        }
      }._2.mkString("(", ", ", ")")

    s"implicit def $methodName $typeInfo = Reduce(s => Node${nodeIds(to)}($baseState, $toState))"
  }

  private lazy val reduceAndGoTo: Map[LRClosure, Map[Terminal, Set[(Option[LRClosure], List[LRClosure], NonTerminal)]]] = automaton.reduce.map { case (from, map) =>
    from -> map.map { case (term, (nt, expr)) =>
      val path = expr.foldRight(Set(List(from))) { (_, set) =>
        set.flatMap { lst => automaton.reverseEdges(lst.head).map(_ :: lst) }
      }
      term -> path.map(list => (automaton.goTo(list.head).get(nt), list, nt))
    }
  }

  private def constructorArgumentListString (nt: NonTerminal): String = {
    constructorArguments(nt).map { case (s, n) => s"arg$n: $s" }.mkString("(", ", ", ")")
  }

  private lazy val constructorArguments : Map[NonTerminal, List[(String, Int)]] = automaton.syntax.rules.collect {
    case DerivationRule(left, right) => left -> right.collect {
      case nt: NonTerminal    => nt.typeName
      case LiteralToken(_, t) => t
    }.zipWithIndex
  } (breakOut)

  private def constructLiteral (literal: LiteralToken, arg: String): String = edgesFromStart.get(literal) match {
    case Some(node) => s"Node${nodeIds(node)}(Node${nodeIds(automaton.start)}, $arg)"
    case None       => s"${symbolTypeNames(literal)}($arg)"
  }

  private def symbolTypeNames (symbol: Symbol): String = symbol match {
    /* 一旦保留
    case lit: LiteralToken if edgesFromStart.contains(lit) =>
      s"Node${nodeIds(edgesFromStart(lit))}[Node${nodeIds(automaton.start)}.type]"*/

    case nt: NonTerminal    => nt.typeName
    case Keyword(kw)        => kw.capitalize + ".type"
    case LiteralToken(_, t) => "ScaLALR$Literal$Token$" + t
    case EndOfInput         => "EoI.type"
    case EmptyString        => throw new Exception("empty string")
  }

  private def terminalNames (terminal: Terminal): String = terminal match {
    case Keyword(kw)        => kw
    case LiteralToken(n, _) => n
    case EndOfInput         => "eoi"
  }

  private lazy val traitNonTerminals: Set[NonTerminal] = automaton.syntax.rules.collect {
    case BranchRule(left, _) => left
  } (breakOut)

  private def superNonTerminal (nt: NonTerminal): Option[NonTerminal] = automaton.syntax.rules.collectFirst {
    case BranchRule(left, right) if right.contains(nt) => left
  }

  private lazy val edgesFromStart = automaton.edges(automaton.start)

  private lazy val nodeIds: Map[LRClosure, Int] = automaton.nodes.zipWithIndex.toMap
}
