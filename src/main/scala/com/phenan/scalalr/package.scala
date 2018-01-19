package com.phenan

import scala.language.implicitConversions

package object scalalr {
  implicit class ScaLALRStringContext (sc: StringContext) {
    def g (args: Any*): ScaLALRString = ScaLALRString(sc.parts, args)
  }

  implicit class TokenListOps [T <: TokenList] (t: T) {
    def apply [U] (value: TokenListCons[Literal[U], TokenListSentinel]): TokenListCons[Literal[U], T] = TokenListCons(value.head, t)
  }

  def singleToken [T] (t: T): TokenListCons[T, TokenListSentinel] = TokenListCons(t, TokenListSentinel)

  implicit def shift_transition [T, N1, N2] (implicit shift: Shift[T, N1, N2]): Transition[T, N1, N2] = Transition ({ (n: N1, t: T) => shift.shift(n, t) })
  implicit def reduce_transition [T, N1, N2, N3] (implicit reduce: Reduce[T, N1, N2], transition: Transition[T, N2, N3]): Transition[T, N1, N3] = Transition { (state, terminal) => transition.transit(reduce.reduce(state), terminal) }
  implicit def accept_transition [NX, R] (implicit accept: Accept[NX, R]): Transition[EoI.type, NX, R] = Transition { (n, _) => accept.accept(n) }
  implicit def accept_ast[NX, R] (node: NX)(implicit transition: Transition[EoI.type, NX, R]): R = transition.transit(node, EoI)
  implicit def simple_transition [T, N1, N2] (implicit transition: Transition[T, N1, N2]): Transitions[TokenListCons[T, TokenListSentinel], N1, N2] = Transitions((n, h) => transition.transit(n, h.head))
  implicit def composed_transitions [T, L <: TokenList, N1, N2, N3] (implicit transitions: Transitions[L, N1, N2], transition: Transition[T, N2, N3]): Transitions[TokenListCons[T, L], N1, N3] = Transitions((n, h) => transition.transit(transitions.transit(n, h.tail), h.head))

  implicit def literal [T] (value: T): TokenListCons[Literal[T], TokenListSentinel] = singleToken(Literal(value))

  def $$semicolon : TokenListCons[EoI.type, TokenListSentinel] = singleToken(EoI)
}
