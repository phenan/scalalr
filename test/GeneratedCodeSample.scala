import com.phenan.scalalr._
import com.phenan.scalalr.internal._

object MathDSL2 {
  
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  sealed trait Expr
  case class Mul (arg0: Term, arg1: Factor) extends Term
  sealed trait Term extends Expr
  case class Stmts (arg0: Stmt, arg1: Program) extends Program
  case class Div (arg0: Term, arg1: Factor) extends Term
  case class Num (arg0: Int) extends Factor
  sealed trait Factor extends Term
  case class Stmt (arg0: Expr) extends Program
  case class Paren (arg0: Expr) extends Factor
  sealed trait Program
  case class Add (arg0: Expr, arg1: Term) extends Expr
  case class Sub (arg0: Expr, arg1: Term) extends Expr
  
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  case class Node0 [NX] (prev: NX, value: Expr)
  case class Node1 [NX] (prev: NX)
  case class Node2 [NX] (prev: NX)
  case class Node3 [NX] (prev: NX)
  case class Node4 [NX] (prev: NX, value: Stmts)
  case class Node5 [NX] (prev: NX, value: Expr)
  case class Node6 [NX] (prev: NX, value: Stmt)
  case class Node7 [NX] (prev: NX, value: Term)
  case class Node8 [NX] (prev: NX, value: Program)
  case class Node9 [NX] (prev: NX)
  case class Node10 [NX] (prev: NX, value: Int)
  case class Node11 [NX] (prev: NX, value: Add)
  case class Node12 [NX] (prev: NX, value: Factor)
  case class Node13 [NX] (prev: NX, value: Sub)
  case object Node14
  case class Node15 [NX] (prev: NX, value: Term)
  case class Node16 [NX] (prev: NX, value: Program)
  case class Node17 [NX] (prev: NX, value: Div)
  case class Node18 [NX] (prev: NX, value: Term)
  case class Node19 [NX] (prev: NX)
  case class Node20 [NX] (prev: NX, value: Mul)
  case class Node21 [NX] (prev: NX, value: Num)
  case class Node22 [NX] (prev: NX)
  case class Node23 [NX] (prev: NX, value: Factor)
  case class Node24 [NX] (prev: NX, value: Paren)
  case class Node25 [NX] (prev: NX, value: Factor)
  case class Node26 [NX] (prev: NX)
  
  //////////////////////////////////////////////////////////////////////////////////////////////////////////

  case object Lp
  case object Mul
  case object Rp
  case object Minus
  case object End
  case object Plus
  case object Div
  
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  def int (value: Int): TokenListCons[Literal[Int], TokenListSentinel] = TokenListCons(Literal(value), TokenListSentinel)

  def $$asterisk : TokenListCons[Mul.type, TokenListSentinel] = singleToken(Mul)
  def $$parenleft : TokenListCons[Lp.type, TokenListSentinel] = singleToken(Lp)
  def $$parenright : TokenListCons[Rp.type, TokenListSentinel] = singleToken(Rp)
  def $$hyphen : TokenListCons[Minus.type, TokenListSentinel] = singleToken(Minus)
  def end : TokenListCons[End.type, TokenListSentinel] = singleToken(End)
  def $$plus : TokenListCons[Plus.type, TokenListSentinel] = singleToken(Plus)
  def $$slash : TokenListCons[Div.type, TokenListSentinel] = singleToken(Div)
  
  //////////////////////////////////////////////////////////////////////////////////////////////////////////

  implicit class start_with_actual_literal_$$plus [T, N1, N2] (value: T) (implicit transition1: Transition[Literal[T], Node14.type, N1], transition2: Transition[Plus.type, N1, N2]) {
    def $$plus : N2 = transition2.transit(transition1.transit(Node14, Literal[T](value)), Plus)
    def $$plus [U, N3] (value2: U)(implicit transition3: Transition[Literal[U], N2, N3]): N3 = transition3.transit($$plus, Literal[U](value2))
    def $$plus [U <: TokenList, N3] (value2: U)(implicit transitions: Transitions[U, N2, N3]): N3 = transitions.transit($$plus, value2)
  }

  implicit class start_with_actual_literal_$$hyphen [T, N1, N2] (value: T) (implicit transition1: Transition[Literal[T], Node14.type, N1], transition2: Transition[Minus.type, N1, N2]) {
    def $$hyphen : N2 = transition2.transit(transition1.transit(Node14, Literal[T](value)), Minus)
    def $$hyphen [U, N3] (value2: U)(implicit transition3: Transition[Literal[U], N2, N3]): N3 = transition3.transit($$hyphen, Literal[U](value2))
    def $$hyphen [U <: TokenList, N3] (value2: U)(implicit transitions: Transitions[U, N2, N3]): N3 = transitions.transit($$hyphen, value2)
  }

  implicit class start_with_actual_literal_$$asterisk [T, N1, N2] (value: T) (implicit transition1: Transition[Literal[T], Node14.type, N1], transition2: Transition[Mul.type, N1, N2]) {
    def $$asterisk : N2 = transition2.transit(transition1.transit(Node14, Literal[T](value)), Mul)
    def $$asterisk [U, N3] (value2: U)(implicit transition3: Transition[Literal[U], N2, N3]): N3 = transition3.transit($$asterisk, Literal[U](value2))
    def $$asterisk [U <: TokenList, N3] (value2: U)(implicit transitions: Transitions[U, N2, N3]): N3 = transitions.transit($$asterisk, value2)
  }

  implicit class start_with_actual_literal_$$slash [T, N1, N2] (value: T) (implicit transition1: Transition[Literal[T], Node14.type, N1], transition2: Transition[Div.type, N1, N2]) {
    def $$slash : N2 = transition2.transit(transition1.transit(Node14, Literal[T](value)), Div)
    def $$slash [U, N3] (value2: U)(implicit transition3: Transition[Literal[U], N2, N3]): N3 = transition3.transit($$slash, Literal[U](value2))
    def $$slash [U <: TokenList, N3] (value2: U)(implicit transitions: Transitions[U, N2, N3]): N3 = transitions.transit($$slash, value2)
  }

  implicit class start_with_actual_literal_$$parenleft [T, N1, N2] (value: T) (implicit transition1: Transition[Literal[T], Node14.type, N1], transition2: Transition[Lp.type, N1, N2]) {
    def $$parenleft : N2 = transition2.transit(transition1.transit(Node14, Literal[T](value)), Lp)
    def $$parenleft [U, N3] (value2: U)(implicit transition3: Transition[Literal[U], N2, N3]): N3 = transition3.transit($$parenleft, Literal[U](value2))
    def $$parenleft [U <: TokenList, N3] (value2: U)(implicit transitions: Transitions[U, N2, N3]): N3 = transitions.transit($$parenleft, value2)
  }

  implicit class start_with_actual_literal_$$parenright [T, N1, N2] (value: T) (implicit transition1: Transition[Literal[T], Node14.type, N1], transition2: Transition[Rp.type, N1, N2]) {
    def $$parenright : N2 = transition2.transit(transition1.transit(Node14, Literal[T](value)), Rp)
    def $$parenright [U, N3] (value2: U)(implicit transition3: Transition[Literal[U], N2, N3]): N3 = transition3.transit($$parenright, Literal[U](value2))
    def $$parenright [U <: TokenList, N3] (value2: U)(implicit transitions: Transitions[U, N2, N3]): N3 = transitions.transit($$parenright, value2)
  }

  implicit class start_with_actual_literal_end [T, N1, N2] (value: T) (implicit transition1: Transition[Literal[T], Node14.type, N1], transition2: Transition[End.type, N1, N2]) {
    def end : N2 = transition2.transit(transition1.transit(Node14, Literal[T](value)), End)
    def end [U, N3] (value2: U)(implicit transition3: Transition[Literal[U], N2, N3]): N3 = transition3.transit(end, Literal[U](value2))
    def end [U <: TokenList, N3] (value2: U)(implicit transitions: Transitions[U, N2, N3]): N3 = transitions.transit(end, value2)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////

  implicit class start_with_function_style_$$plus [L <: TokenList, N1, N2] (tokens: L)(implicit transitions: Transitions[L, Node14.type, N1], transition1: Transition[Plus.type, N1, N2]) {
    def $$plus : N2 = transition1.transit(transitions.transit(Node14, tokens), Plus)
    def $$plus [U, N3] (value: U)(implicit transition2: Transition[Literal[U], N2, N3]): N3 = transition2.transit($$plus, Literal[U](value))
    def $$plus [U <: TokenList, N3] (value: U)(implicit transitions2: Transitions[U, N2, N3]): N3 = transitions2.transit($$plus, value)
  }

  implicit class start_with_function_style_$$hyphen [L <: TokenList, N1, N2] (tokens: L)(implicit transitions: Transitions[L, Node14.type, N1], transition1: Transition[Minus.type, N1, N2]) {
    def $$hyphen : N2 = transition1.transit(transitions.transit(Node14, tokens), Minus)
    def $$hyphen [U, N3] (value: U)(implicit transition2: Transition[Literal[U], N2, N3]): N3 = transition2.transit($$hyphen, Literal[U](value))
    def $$hyphen [U <: TokenList, N3] (value: U)(implicit transitions2: Transitions[U, N2, N3]): N3 = transitions2.transit($$hyphen, value)
  }

  implicit class start_with_function_style_$$asterisk [L <: TokenList, N1, N2] (tokens: L)(implicit transitions: Transitions[L, Node14.type, N1], transition1: Transition[Mul.type, N1, N2]) {
    def $$asterisk : N2 = transition1.transit(transitions.transit(Node14, tokens), Mul)
    def $$asterisk [U, N3] (value: U)(implicit transition2: Transition[Literal[U], N2, N3]): N3 = transition2.transit($$asterisk, Literal[U](value))
    def $$asterisk [U <: TokenList, N3] (value: U)(implicit transitions2: Transitions[U, N2, N3]): N3 = transitions2.transit($$asterisk, value)
  }

  implicit class start_with_function_style_$$slash [L <: TokenList, N1, N2] (tokens: L)(implicit transitions: Transitions[L, Node14.type, N1], transition1: Transition[Div.type, N1, N2]) {
    def $$slash : N2 = transition1.transit(transitions.transit(Node14, tokens), Div)
    def $$slash [U, N3] (value: U)(implicit transition2: Transition[Literal[U], N2, N3]): N3 = transition2.transit($$slash, Literal[U](value))
    def $$slash [U <: TokenList, N3] (value: U)(implicit transitions2: Transitions[U, N2, N3]): N3 = transitions2.transit($$slash, value)
  }

  implicit class start_with_function_style_$$parenleft [L <: TokenList, N1, N2] (tokens: L)(implicit transitions: Transitions[L, Node14.type, N1], transition1: Transition[Lp.type, N1, N2]) {
    def $$parenleft : N2 = transition1.transit(transitions.transit(Node14, tokens), Lp)
    def $$parenleft [U, N3] (value: U)(implicit transition2: Transition[Literal[U], N2, N3]): N3 = transition2.transit($$parenleft, Literal[U](value))
    def $$parenleft [U <: TokenList, N3] (value: U)(implicit transitions2: Transitions[U, N2, N3]): N3 = transitions2.transit($$parenleft, value)
  }

  implicit class start_with_function_style_$$parenright [L <: TokenList, N1, N2] (tokens: L)(implicit transitions: Transitions[L, Node14.type, N1], transition1: Transition[Rp.type, N1, N2]) {
    def $$parenright : N2 = transition1.transit(transitions.transit(Node14, tokens), Rp)
    def $$parenright [U, N3] (value: U)(implicit transition2: Transition[Literal[U], N2, N3]): N3 = transition2.transit($$parenright, Literal[U](value))
    def $$parenright [U <: TokenList, N3] (value: U)(implicit transitions2: Transitions[U, N2, N3]): N3 = transitions2.transit($$parenright, value)
  }

  implicit class start_with_function_style_end [L <: TokenList, N1, N2] (tokens: L)(implicit transitions: Transitions[L, Node14.type, N1], transition1: Transition[End.type, N1, N2]) {
    def end : N2 = transition1.transit(transitions.transit(Node14, tokens), End)
    def end [U, N3] (value: U)(implicit transition2: Transition[Literal[U], N2, N3]): N3 = transition2.transit(end, Literal[U](value))
    def end [U <: TokenList, N3] (value: U)(implicit transitions2: Transitions[U, N2, N3]): N3 = transitions2.transit(end, value)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////

  implicit class transition_$$parenleft [N1, N2] (node: N1) (implicit transition1: Transition[Lp.type, N1, N2]) {
    def $$parenleft : N2 = transition1.transit(node, Lp)
    def $$parenleft [T, N3] (value: T)(implicit transition2: Transition[Literal[T], N2, N3]): N3 = transition2.transit($$parenleft, Literal[T](value))
    def $$parenleft [H <: TokenList, N3] (tokens: H)(implicit transitions: Transitions[H, N2, N3]): N3 = transitions.transit($$parenleft, tokens)
  }
  implicit class transition_$$asterisk [N1, N2] (node: N1) (implicit transition1: Transition[Mul.type, N1, N2]) {
    def $$asterisk : N2 = transition1.transit(node, Mul)
    def $$asterisk [T, N3] (value: T)(implicit transition2: Transition[Literal[T], N2, N3]): N3 = transition2.transit($$asterisk, Literal[T](value))
    def $$asterisk [H <: TokenList, N3] (tokens: H)(implicit transitions: Transitions[H, N2, N3]): N3 = transitions.transit($$asterisk, tokens)
  }
  implicit class transition_$$parenright [N1, N2] (node: N1) (implicit transition1: Transition[Rp.type, N1, N2]) {
    def $$parenright : N2 = transition1.transit(node, Rp)
    def $$parenright [T, N3] (value: T)(implicit transition2: Transition[Literal[T], N2, N3]): N3 = transition2.transit($$parenright, Literal[T](value))
    def $$parenright [H <: TokenList, N3] (tokens: H)(implicit transitions: Transitions[H, N2, N3]): N3 = transitions.transit($$parenright, tokens)
  }
  implicit class transition_$$hyphen [N1, N2] (node: N1) (implicit transition1: Transition[Minus.type, N1, N2]) {
    def $$hyphen : N2 = transition1.transit(node, Minus)
    def $$hyphen [T, N3] (value: T)(implicit transition2: Transition[Literal[T], N2, N3]): N3 = transition2.transit($$hyphen, Literal[T](value))
    def $$hyphen [H <: TokenList, N3] (tokens: H)(implicit transitions: Transitions[H, N2, N3]): N3 = transitions.transit($$hyphen, tokens)
  }
  implicit class transition_end [N1, N2] (node: N1) (implicit transition1: Transition[End.type, N1, N2]) {
    def end : N2 = transition1.transit(node, End)
    def end [T, N3] (value: T)(implicit transition2: Transition[Literal[T], N2, N3]): N3 = transition2.transit(end, Literal[T](value))
    def end [H <: TokenList, N3] (tokens: H)(implicit transitions: Transitions[H, N2, N3]): N3 = transitions.transit(end, tokens)
  }
  implicit class transition_$$plus [N1, N2] (node: N1) (implicit transition1: Transition[Plus.type, N1, N2]) {
    def $$plus : N2 = transition1.transit(node, Plus)
    def $$plus [T, N3] (value: T)(implicit transition2: Transition[Literal[T], N2, N3]): N3 = transition2.transit($$plus, Literal[T](value))
    def $$plus [H <: TokenList, N3] (tokens: H)(implicit transitions: Transitions[H, N2, N3]): N3 = transitions.transit($$plus, tokens)
  }
  implicit class transition_$$slash [N1, N2] (node: N1) (implicit transition1: Transition[Div.type, N1, N2]) {
    def $$slash : N2 = transition1.transit(node, Div)
    def $$slash [T, N3] (value: T)(implicit transition2: Transition[Literal[T], N2, N3]): N3 = transition2.transit($$slash, Literal[T](value))
    def $$slash [H <: TokenList, N3] (tokens: H)(implicit transitions: Transitions[H, N2, N3]): N3 = transitions.transit($$slash, tokens)
  }

  /* こちらだとダメ
  implicit class transition_keyword [N1] (node: N1) {
    def $$parenleft [N2] (implicit transition1: Transition[Lp.type, N1, N2]): N2 = transition1.transit(node, Lp)
    def $$parenleft [T, N2, N3] (value: T)(implicit transition1: Transition[Lp.type, N1, N2], transition2: Transition[Literal[T], N2, N3]): N3 = transition2.transit($$parenleft, Literal[T](value))
    def $$parenleft [H <: TokenList, N2, N3] (tokens: H)(implicit transition1: Transition[Lp.type, N1, N2], transitions: Transitions[H, N2, N3]): N3 = transitions.transit($$parenleft, tokens)

    def $$asterisk [N2] (implicit transition1: Transition[Mul.type, N1, N2]): N2 = transition1.transit(node, Mul)
    def $$asterisk [H <: TokenList, N2, N3] (tokens: H)(implicit transition1: Transition[Mul.type, N1, N2], transitions: Transitions[H, N2, N3]): N3 = transitions.transit($$asterisk, tokens)

    def $$parenright [N2] (implicit transition1: Transition[Rp.type, N1, N2]): N2 = transition1.transit(node, Rp)
    def $$parenright [H <: TokenList, N2, N3] (tokens: H)(implicit transition1: Transition[Rp.type, N1, N2], transitions: Transitions[H, N2, N3]): N3 = transitions.transit($$parenright, tokens)

    def $$hyphen [N2] (implicit transition1: Transition[Minus.type, N1, N2]): N2 = transition1.transit(node, Minus)
    def $$hyphen [H <: TokenList, N2, N3] (tokens: H)(implicit transition1: Transition[Minus.type, N1, N2], transitions: Transitions[H, N2, N3]): N3 = transitions.transit($$hyphen, tokens)

    def end [N2] (implicit transition1: Transition[End.type, N1, N2]): N2 = transition1.transit(node, End)
    def end [H <: TokenList, N2, N3] (tokens: H)(implicit transition1: Transition[End.type, N1, N2], transitions: Transitions[H, N2, N3]): N3 = transitions.transit(end, tokens)

    def $$plus [N2] (implicit transition1: Transition[Plus.type, N1, N2]): N2 = transition1.transit(node, Plus)
    def $$plus [T, N2, N3] (value: T)(implicit transition1: Transition[Plus.type, N1, N2], transition2: Transition[Literal[T], N2, N3]): N3 = transition2.transit($$plus, Literal[T](value))
    def $$plus [H <: TokenList, N2, N3] (tokens: H)(implicit transition1: Transition[Plus.type, N1, N2], transitions: Transitions[H, N2, N3]): N3 = transitions.transit($$plus, tokens)

    def $$slash [N2] (implicit transition1: Transition[Div.type, N1, N2]): N2 = transition1.transit(node, Div)
    def $$slash [T, N2, N3] (value: T)(implicit transition1: Transition[Div.type, N1, N2], transition2: Transition[Literal[T], N2, N3]): N3 = transition2.transit($$slash, Literal[T](value))
    def $$slash [H <: TokenList, N2, N3] (tokens: H)(implicit transition1: Transition[Div.type, N1, N2], transitions: Transitions[H, N2, N3]): N3 = transitions.transit($$slash, tokens)
  }*/
  
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  implicit class transition_int [N1, N2] (node: N1) (implicit transition: Transition[Literal[Int], N1, N2]) {
    def int (value: Int): N2 = transition.transit(node, Literal[Int](value))
    //def apply (value: Int): N2 = transition.transit(node, Node10(Node14, value))
  }
  
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  implicit def node0_shift_node2 [NX] : Shift[End.type, Node0[NX], Node2[Node0[NX]]] = Shift((s, _) => Node2(s))
  implicit def node0_shift_node19 [NX] : Shift[Minus.type, Node0[NX], Node19[Node0[NX]]] = Shift((s, _) => Node19(s))
  implicit def node0_shift_node3 [NX] : Shift[Plus.type, Node0[NX], Node3[Node0[NX]]] = Shift((s, _) => Node3(s))
  implicit def node3_shift_node10 [NX] : Shift[Literal[Int], Node3[NX], Node10[Node3[NX]]] = Shift((s, t) => Node10(s, t.value))
  implicit def node3_shift_node9 [NX] : Shift[Lp.type, Node3[NX], Node9[Node3[NX]]] = Shift((s, _) => Node9(s))
  implicit def node5_shift_node1 [NX] : Shift[Rp.type, Node5[NX], Node1[Node5[NX]]] = Shift((s, _) => Node1(s))
  implicit def node5_shift_node19 [NX] : Shift[Minus.type, Node5[NX], Node19[Node5[NX]]] = Shift((s, _) => Node19(s))
  implicit def node5_shift_node3 [NX] : Shift[Plus.type, Node5[NX], Node3[Node5[NX]]] = Shift((s, _) => Node3(s))
  implicit def node6_shift_node10 [NX] : Shift[Literal[Int], Node6[NX], Node10[Node6[NX]]] = Shift((s, t) => Node10(s, t.value))
  implicit def node6_shift_node9 [NX] : Shift[Lp.type, Node6[NX], Node9[Node6[NX]]] = Shift((s, _) => Node9(s))
  implicit def node7_shift_node22 [NX] : Shift[Div.type, Node7[NX], Node22[Node7[NX]]] = Shift((s, _) => Node22(s))
  implicit def node7_shift_node26 [NX] : Shift[Mul.type, Node7[NX], Node26[Node7[NX]]] = Shift((s, _) => Node26(s))
  implicit def node9_shift_node10 [NX] : Shift[Literal[Int], Node9[NX], Node10[Node9[NX]]] = Shift((s, t) => Node10(s, t.value))
  implicit def node9_shift_node9 [NX] : Shift[Lp.type, Node9[NX], Node9[Node9[NX]]] = Shift((s, _) => Node9(s))
  implicit def node14_shift_node10 : Shift[Literal[Int], Node14.type, Node10[Node14.type]] = Shift((s, t) => Node10[Node14.type](s, t.value))
  implicit def node14_shift_node9 : Shift[Lp.type, Node14.type, Node9[Node14.type]] = Shift((s, _) => Node9[Node14.type](s))
  implicit def node15_shift_node22 [NX] : Shift[Div.type, Node15[NX], Node22[Node15[NX]]] = Shift((s, _) => Node22(s))
  implicit def node15_shift_node26 [NX] : Shift[Mul.type, Node15[NX], Node26[Node15[NX]]] = Shift((s, _) => Node26(s))
  implicit def node18_shift_node22 [NX] : Shift[Div.type, Node18[NX], Node22[Node18[NX]]] = Shift((s, _) => Node22(s))
  implicit def node18_shift_node26 [NX] : Shift[Mul.type, Node18[NX], Node26[Node18[NX]]] = Shift((s, _) => Node26(s))
  implicit def node19_shift_node10 [NX] : Shift[Literal[Int], Node19[NX], Node10[Node19[NX]]] = Shift((s, t) => Node10(s, t.value))
  implicit def node19_shift_node9 [NX] : Shift[Lp.type, Node19[NX], Node9[Node19[NX]]] = Shift((s, _) => Node9(s))
  implicit def node22_shift_node10 [NX] : Shift[Literal[Int], Node22[NX], Node10[Node22[NX]]] = Shift((s, t) => Node10(s, t.value))
  implicit def node22_shift_node9 [NX] : Shift[Lp.type, Node22[NX], Node9[Node22[NX]]] = Shift((s, _) => Node9(s))
  implicit def node26_shift_node10 [NX] : Shift[Literal[Int], Node26[NX], Node10[Node26[NX]]] = Shift((s, t) => Node10(s, t.value))
  implicit def node26_shift_node9 [NX] : Shift[Lp.type, Node26[NX], Node9[Node26[NX]]] = Shift((s, _) => Node9(s))
  implicit def node1_reduce_node24_$$asterisk_3_9_5 [NX] : Reduce[Mul.type, Node1[Node5[Node9[Node3[NX]]]], Node24[Node3[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$asterisk_6_9_5 [NX] : Reduce[Mul.type, Node1[Node5[Node9[Node6[NX]]]], Node24[Node6[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$asterisk_14_9_5 : Reduce[Mul.type, Node1[Node5[Node9[Node14.type]]], Node24[Node14.type]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$asterisk_26_9_5 [NX] : Reduce[Mul.type, Node1[Node5[Node9[Node26[NX]]]], Node24[Node26[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$asterisk_19_9_5 [NX] : Reduce[Mul.type, Node1[Node5[Node9[Node19[NX]]]], Node24[Node19[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$asterisk_9_9_5 [NX] : Reduce[Mul.type, Node1[Node5[Node9[Node9[NX]]]], Node24[Node9[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$asterisk_22_9_5 [NX] : Reduce[Mul.type, Node1[Node5[Node9[Node22[NX]]]], Node24[Node22[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$parenright_14_9_5 : Reduce[Rp.type, Node1[Node5[Node9[Node14.type]]], Node24[Node14.type]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$parenright_19_9_5 [NX] : Reduce[Rp.type, Node1[Node5[Node9[Node19[NX]]]], Node24[Node19[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$parenright_3_9_5 [NX] : Reduce[Rp.type, Node1[Node5[Node9[Node3[NX]]]], Node24[Node3[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$parenright_9_9_5 [NX] : Reduce[Rp.type, Node1[Node5[Node9[Node9[NX]]]], Node24[Node9[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$parenright_6_9_5 [NX] : Reduce[Rp.type, Node1[Node5[Node9[Node6[NX]]]], Node24[Node6[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$parenright_26_9_5 [NX] : Reduce[Rp.type, Node1[Node5[Node9[Node26[NX]]]], Node24[Node26[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$parenright_22_9_5 [NX] : Reduce[Rp.type, Node1[Node5[Node9[Node22[NX]]]], Node24[Node22[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$hyphen_6_9_5 [NX] : Reduce[Minus.type, Node1[Node5[Node9[Node6[NX]]]], Node24[Node6[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$hyphen_22_9_5 [NX] : Reduce[Minus.type, Node1[Node5[Node9[Node22[NX]]]], Node24[Node22[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$hyphen_9_9_5 [NX] : Reduce[Minus.type, Node1[Node5[Node9[Node9[NX]]]], Node24[Node9[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$hyphen_3_9_5 [NX] : Reduce[Minus.type, Node1[Node5[Node9[Node3[NX]]]], Node24[Node3[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$hyphen_26_9_5 [NX] : Reduce[Minus.type, Node1[Node5[Node9[Node26[NX]]]], Node24[Node26[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$hyphen_14_9_5 : Reduce[Minus.type, Node1[Node5[Node9[Node14.type]]], Node24[Node14.type]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$hyphen_19_9_5 [NX] : Reduce[Minus.type, Node1[Node5[Node9[Node19[NX]]]], Node24[Node19[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_end_19_9_5 [NX] : Reduce[End.type, Node1[Node5[Node9[Node19[NX]]]], Node24[Node19[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_end_22_9_5 [NX] : Reduce[End.type, Node1[Node5[Node9[Node22[NX]]]], Node24[Node22[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_end_6_9_5 [NX] : Reduce[End.type, Node1[Node5[Node9[Node6[NX]]]], Node24[Node6[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_end_26_9_5 [NX] : Reduce[End.type, Node1[Node5[Node9[Node26[NX]]]], Node24[Node26[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_end_14_9_5 : Reduce[End.type, Node1[Node5[Node9[Node14.type]]], Node24[Node14.type]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_end_3_9_5 [NX] : Reduce[End.type, Node1[Node5[Node9[Node3[NX]]]], Node24[Node3[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_end_9_9_5 [NX] : Reduce[End.type, Node1[Node5[Node9[Node9[NX]]]], Node24[Node9[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$plus_3_9_5 [NX] : Reduce[Plus.type, Node1[Node5[Node9[Node3[NX]]]], Node24[Node3[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$plus_6_9_5 [NX] : Reduce[Plus.type, Node1[Node5[Node9[Node6[NX]]]], Node24[Node6[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$plus_22_9_5 [NX] : Reduce[Plus.type, Node1[Node5[Node9[Node22[NX]]]], Node24[Node22[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$plus_19_9_5 [NX] : Reduce[Plus.type, Node1[Node5[Node9[Node19[NX]]]], Node24[Node19[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$plus_9_9_5 [NX] : Reduce[Plus.type, Node1[Node5[Node9[Node9[NX]]]], Node24[Node9[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$plus_26_9_5 [NX] : Reduce[Plus.type, Node1[Node5[Node9[Node26[NX]]]], Node24[Node26[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$plus_14_9_5 : Reduce[Plus.type, Node1[Node5[Node9[Node14.type]]], Node24[Node14.type]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$slash_19_9_5 [NX] : Reduce[Div.type, Node1[Node5[Node9[Node19[NX]]]], Node24[Node19[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$slash_26_9_5 [NX] : Reduce[Div.type, Node1[Node5[Node9[Node26[NX]]]], Node24[Node26[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$slash_14_9_5 : Reduce[Div.type, Node1[Node5[Node9[Node14.type]]], Node24[Node14.type]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$slash_22_9_5 [NX] : Reduce[Div.type, Node1[Node5[Node9[Node22[NX]]]], Node24[Node22[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$slash_3_9_5 [NX] : Reduce[Div.type, Node1[Node5[Node9[Node3[NX]]]], Node24[Node3[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$slash_6_9_5 [NX] : Reduce[Div.type, Node1[Node5[Node9[Node6[NX]]]], Node24[Node6[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node1_reduce_node24_$$slash_9_9_5 [NX] : Reduce[Div.type, Node1[Node5[Node9[Node9[NX]]]], Node24[Node9[NX]]] = Reduce(s => Node24(s.prev.prev.prev, Paren(s.prev.value)))
  implicit def node2_reduce_node6_eoi_6_0 [NX] : Reduce[EoI.type, Node2[Node0[Node6[NX]]], Node6[Node6[NX]]] = Reduce(s => Node6(s.prev.prev, Stmt(s.prev.value)))
  implicit def node2_reduce_node6_eoi_14_0 : Reduce[EoI.type, Node2[Node0[Node14.type]], Node6[Node14.type]] = Reduce(s => Node6(s.prev.prev, Stmt(s.prev.value)))
  implicit def node2_reduce_node6_int_6_0 [NX] : Reduce[Literal[Int], Node2[Node0[Node6[NX]]], Node6[Node6[NX]]] = Reduce(s => Node6(s.prev.prev, Stmt(s.prev.value)))
  implicit def node2_reduce_node6_int_14_0 : Reduce[Literal[Int], Node2[Node0[Node14.type]], Node6[Node14.type]] = Reduce(s => Node6(s.prev.prev, Stmt(s.prev.value)))
  implicit def node2_reduce_node6_$$parenleft_6_0 [NX] : Reduce[Lp.type, Node2[Node0[Node6[NX]]], Node6[Node6[NX]]] = Reduce(s => Node6(s.prev.prev, Stmt(s.prev.value)))
  implicit def node2_reduce_node6_$$parenleft_14_0 : Reduce[Lp.type, Node2[Node0[Node14.type]], Node6[Node14.type]] = Reduce(s => Node6(s.prev.prev, Stmt(s.prev.value)))
  implicit def node4_reduce_node16_eoi_6 [NX] : Reduce[EoI.type, Node4[Node6[NX]], Node16[Node6[NX]]] = Reduce(s => Node16(s.prev, s.value))
  implicit def node4_reduce_node8_eoi_14 : Reduce[EoI.type, Node4[Node14.type], Node8[Node14.type]] = Reduce(s => Node8(s.prev, s.value))
  implicit def node6_reduce_node16_eoi_6 [NX] : Reduce[EoI.type, Node6[Node6[NX]], Node16[Node6[NX]]] = Reduce(s => Node16(s.prev, s.value))
  implicit def node6_reduce_node8_eoi_14 : Reduce[EoI.type, Node6[Node14.type], Node8[Node14.type]] = Reduce(s => Node8(s.prev, s.value))
  implicit def node7_reduce_node0_end_6 [NX] : Reduce[End.type, Node7[Node6[NX]], Node0[Node6[NX]]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node7_reduce_node5_end_9 [NX] : Reduce[End.type, Node7[Node9[NX]], Node5[Node9[NX]]] = Reduce(s => Node5(s.prev, s.value))
  implicit def node7_reduce_node0_end_14 : Reduce[End.type, Node7[Node14.type], Node0[Node14.type]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node7_reduce_node0_$$hyphen_6 [NX] : Reduce[Minus.type, Node7[Node6[NX]], Node0[Node6[NX]]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node7_reduce_node5_$$hyphen_9 [NX] : Reduce[Minus.type, Node7[Node9[NX]], Node5[Node9[NX]]] = Reduce(s => Node5(s.prev, s.value))
  implicit def node7_reduce_node0_$$hyphen_14 : Reduce[Minus.type, Node7[Node14.type], Node0[Node14.type]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node7_reduce_node0_$$plus_6 [NX] : Reduce[Plus.type, Node7[Node6[NX]], Node0[Node6[NX]]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node7_reduce_node5_$$plus_9 [NX] : Reduce[Plus.type, Node7[Node9[NX]], Node5[Node9[NX]]] = Reduce(s => Node5(s.prev, s.value))
  implicit def node7_reduce_node0_$$plus_14 : Reduce[Plus.type, Node7[Node14.type], Node0[Node14.type]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node7_reduce_node0_$$parenright_6 [NX] : Reduce[Rp.type, Node7[Node6[NX]], Node0[Node6[NX]]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node7_reduce_node5_$$parenright_9 [NX] : Reduce[Rp.type, Node7[Node9[NX]], Node5[Node9[NX]]] = Reduce(s => Node5(s.prev, s.value))
  implicit def node7_reduce_node0_$$parenright_14 : Reduce[Rp.type, Node7[Node14.type], Node0[Node14.type]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node8_accept[NX]: Accept[Node8[NX], Program] = Accept(s => s.value)
  implicit def node10_reduce_node21_$$asterisk_26 [NX] : Reduce[Mul.type, Node10[Node26[NX]], Node21[Node26[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$asterisk_19 [NX] : Reduce[Mul.type, Node10[Node19[NX]], Node21[Node19[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$asterisk_14 : Reduce[Mul.type, Node10[Node14.type], Node21[Node14.type]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$asterisk_3 [NX] : Reduce[Mul.type, Node10[Node3[NX]], Node21[Node3[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$asterisk_6 [NX] : Reduce[Mul.type, Node10[Node6[NX]], Node21[Node6[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$asterisk_22 [NX] : Reduce[Mul.type, Node10[Node22[NX]], Node21[Node22[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$asterisk_9 [NX] : Reduce[Mul.type, Node10[Node9[NX]], Node21[Node9[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$parenright_26 [NX] : Reduce[Rp.type, Node10[Node26[NX]], Node21[Node26[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$parenright_22 [NX] : Reduce[Rp.type, Node10[Node22[NX]], Node21[Node22[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$parenright_14 : Reduce[Rp.type, Node10[Node14.type], Node21[Node14.type]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$parenright_19 [NX] : Reduce[Rp.type, Node10[Node19[NX]], Node21[Node19[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$parenright_9 [NX] : Reduce[Rp.type, Node10[Node9[NX]], Node21[Node9[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$parenright_6 [NX] : Reduce[Rp.type, Node10[Node6[NX]], Node21[Node6[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$parenright_3 [NX] : Reduce[Rp.type, Node10[Node3[NX]], Node21[Node3[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$hyphen_22 [NX] : Reduce[Minus.type, Node10[Node22[NX]], Node21[Node22[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$hyphen_19 [NX] : Reduce[Minus.type, Node10[Node19[NX]], Node21[Node19[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$hyphen_6 [NX] : Reduce[Minus.type, Node10[Node6[NX]], Node21[Node6[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$hyphen_3 [NX] : Reduce[Minus.type, Node10[Node3[NX]], Node21[Node3[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$hyphen_14 : Reduce[Minus.type, Node10[Node14.type], Node21[Node14.type]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$hyphen_26 [NX] : Reduce[Minus.type, Node10[Node26[NX]], Node21[Node26[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$hyphen_9 [NX] : Reduce[Minus.type, Node10[Node9[NX]], Node21[Node9[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_end_6 [NX] : Reduce[End.type, Node10[Node6[NX]], Node21[Node6[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_end_26 [NX] : Reduce[End.type, Node10[Node26[NX]], Node21[Node26[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_end_3 [NX] : Reduce[End.type, Node10[Node3[NX]], Node21[Node3[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_end_19 [NX] : Reduce[End.type, Node10[Node19[NX]], Node21[Node19[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_end_9 [NX] : Reduce[End.type, Node10[Node9[NX]], Node21[Node9[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_end_22 [NX] : Reduce[End.type, Node10[Node22[NX]], Node21[Node22[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_end_14 : Reduce[End.type, Node10[Node14.type], Node21[Node14.type]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$plus_9 [NX] : Reduce[Plus.type, Node10[Node9[NX]], Node21[Node9[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$plus_14 : Reduce[Plus.type, Node10[Node14.type], Node21[Node14.type]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$plus_6 [NX] : Reduce[Plus.type, Node10[Node6[NX]], Node21[Node6[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$plus_19 [NX] : Reduce[Plus.type, Node10[Node19[NX]], Node21[Node19[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$plus_26 [NX] : Reduce[Plus.type, Node10[Node26[NX]], Node21[Node26[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$plus_3 [NX] : Reduce[Plus.type, Node10[Node3[NX]], Node21[Node3[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$plus_22 [NX] : Reduce[Plus.type, Node10[Node22[NX]], Node21[Node22[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$slash_22 [NX] : Reduce[Div.type, Node10[Node22[NX]], Node21[Node22[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$slash_26 [NX] : Reduce[Div.type, Node10[Node26[NX]], Node21[Node26[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$slash_14 : Reduce[Div.type, Node10[Node14.type], Node21[Node14.type]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$slash_6 [NX] : Reduce[Div.type, Node10[Node6[NX]], Node21[Node6[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$slash_3 [NX] : Reduce[Div.type, Node10[Node3[NX]], Node21[Node3[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$slash_9 [NX] : Reduce[Div.type, Node10[Node9[NX]], Node21[Node9[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node10_reduce_node21_$$slash_19 [NX] : Reduce[Div.type, Node10[Node19[NX]], Node21[Node19[NX]]] = Reduce(s => Node21(s.prev, Num(s.value)))
  implicit def node11_reduce_node0_end_6 [NX] : Reduce[End.type, Node11[Node6[NX]], Node0[Node6[NX]]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node11_reduce_node5_end_9 [NX] : Reduce[End.type, Node11[Node9[NX]], Node5[Node9[NX]]] = Reduce(s => Node5(s.prev, s.value))
  implicit def node11_reduce_node0_end_14 : Reduce[End.type, Node11[Node14.type], Node0[Node14.type]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node11_reduce_node0_$$hyphen_6 [NX] : Reduce[Minus.type, Node11[Node6[NX]], Node0[Node6[NX]]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node11_reduce_node5_$$hyphen_9 [NX] : Reduce[Minus.type, Node11[Node9[NX]], Node5[Node9[NX]]] = Reduce(s => Node5(s.prev, s.value))
  implicit def node11_reduce_node0_$$hyphen_14 : Reduce[Minus.type, Node11[Node14.type], Node0[Node14.type]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node11_reduce_node0_$$plus_6 [NX] : Reduce[Plus.type, Node11[Node6[NX]], Node0[Node6[NX]]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node11_reduce_node5_$$plus_9 [NX] : Reduce[Plus.type, Node11[Node9[NX]], Node5[Node9[NX]]] = Reduce(s => Node5(s.prev, s.value))
  implicit def node11_reduce_node0_$$plus_14 : Reduce[Plus.type, Node11[Node14.type], Node0[Node14.type]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node11_reduce_node0_$$parenright_6 [NX] : Reduce[Rp.type, Node11[Node6[NX]], Node0[Node6[NX]]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node11_reduce_node5_$$parenright_9 [NX] : Reduce[Rp.type, Node11[Node9[NX]], Node5[Node9[NX]]] = Reduce(s => Node5(s.prev, s.value))
  implicit def node11_reduce_node0_$$parenright_14 : Reduce[Rp.type, Node11[Node14.type], Node0[Node14.type]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node12_reduce_node20_$$asterisk_6_7_26 [NX] : Reduce[Mul.type, Node12[Node26[Node7[Node6[NX]]]], Node20[Node6[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$asterisk_3_15_26 [NX] : Reduce[Mul.type, Node12[Node26[Node15[Node3[NX]]]], Node20[Node3[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$asterisk_14_7_26 : Reduce[Mul.type, Node12[Node26[Node7[Node14.type]]], Node20[Node14.type]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$asterisk_19_18_26 [NX] : Reduce[Mul.type, Node12[Node26[Node18[Node19[NX]]]], Node20[Node19[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$asterisk_9_7_26 [NX] : Reduce[Mul.type, Node12[Node26[Node7[Node9[NX]]]], Node20[Node9[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$parenright_19_18_26 [NX] : Reduce[Rp.type, Node12[Node26[Node18[Node19[NX]]]], Node20[Node19[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$parenright_6_7_26 [NX] : Reduce[Rp.type, Node12[Node26[Node7[Node6[NX]]]], Node20[Node6[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$parenright_14_7_26 : Reduce[Rp.type, Node12[Node26[Node7[Node14.type]]], Node20[Node14.type]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$parenright_9_7_26 [NX] : Reduce[Rp.type, Node12[Node26[Node7[Node9[NX]]]], Node20[Node9[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$parenright_3_15_26 [NX] : Reduce[Rp.type, Node12[Node26[Node15[Node3[NX]]]], Node20[Node3[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$hyphen_3_15_26 [NX] : Reduce[Minus.type, Node12[Node26[Node15[Node3[NX]]]], Node20[Node3[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$hyphen_9_7_26 [NX] : Reduce[Minus.type, Node12[Node26[Node7[Node9[NX]]]], Node20[Node9[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$hyphen_19_18_26 [NX] : Reduce[Minus.type, Node12[Node26[Node18[Node19[NX]]]], Node20[Node19[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$hyphen_14_7_26 : Reduce[Minus.type, Node12[Node26[Node7[Node14.type]]], Node20[Node14.type]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$hyphen_6_7_26 [NX] : Reduce[Minus.type, Node12[Node26[Node7[Node6[NX]]]], Node20[Node6[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_end_6_7_26 [NX] : Reduce[End.type, Node12[Node26[Node7[Node6[NX]]]], Node20[Node6[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_end_9_7_26 [NX] : Reduce[End.type, Node12[Node26[Node7[Node9[NX]]]], Node20[Node9[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_end_14_7_26 : Reduce[End.type, Node12[Node26[Node7[Node14.type]]], Node20[Node14.type]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_end_3_15_26 [NX] : Reduce[End.type, Node12[Node26[Node15[Node3[NX]]]], Node20[Node3[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_end_19_18_26 [NX] : Reduce[End.type, Node12[Node26[Node18[Node19[NX]]]], Node20[Node19[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$plus_3_15_26 [NX] : Reduce[Plus.type, Node12[Node26[Node15[Node3[NX]]]], Node20[Node3[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$plus_9_7_26 [NX] : Reduce[Plus.type, Node12[Node26[Node7[Node9[NX]]]], Node20[Node9[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$plus_14_7_26 : Reduce[Plus.type, Node12[Node26[Node7[Node14.type]]], Node20[Node14.type]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$plus_6_7_26 [NX] : Reduce[Plus.type, Node12[Node26[Node7[Node6[NX]]]], Node20[Node6[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$plus_19_18_26 [NX] : Reduce[Plus.type, Node12[Node26[Node18[Node19[NX]]]], Node20[Node19[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$slash_3_15_26 [NX] : Reduce[Div.type, Node12[Node26[Node15[Node3[NX]]]], Node20[Node3[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$slash_19_18_26 [NX] : Reduce[Div.type, Node12[Node26[Node18[Node19[NX]]]], Node20[Node19[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$slash_9_7_26 [NX] : Reduce[Div.type, Node12[Node26[Node7[Node9[NX]]]], Node20[Node9[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$slash_6_7_26 [NX] : Reduce[Div.type, Node12[Node26[Node7[Node6[NX]]]], Node20[Node6[NX]]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node12_reduce_node20_$$slash_14_7_26 : Reduce[Div.type, Node12[Node26[Node7[Node14.type]]], Node20[Node14.type]] = Reduce(s => Node20(s.prev.prev.prev, Mul(s.prev.prev.value, s.value)))
  implicit def node13_reduce_node0_$$parenright_6 [NX] : Reduce[Rp.type, Node13[Node6[NX]], Node0[Node6[NX]]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node13_reduce_node5_$$parenright_9 [NX] : Reduce[Rp.type, Node13[Node9[NX]], Node5[Node9[NX]]] = Reduce(s => Node5(s.prev, s.value))
  implicit def node13_reduce_node0_$$parenright_14 : Reduce[Rp.type, Node13[Node14.type], Node0[Node14.type]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node13_reduce_node0_$$hyphen_6 [NX] : Reduce[Minus.type, Node13[Node6[NX]], Node0[Node6[NX]]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node13_reduce_node5_$$hyphen_9 [NX] : Reduce[Minus.type, Node13[Node9[NX]], Node5[Node9[NX]]] = Reduce(s => Node5(s.prev, s.value))
  implicit def node13_reduce_node0_$$hyphen_14 : Reduce[Minus.type, Node13[Node14.type], Node0[Node14.type]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node13_reduce_node0_$$plus_6 [NX] : Reduce[Plus.type, Node13[Node6[NX]], Node0[Node6[NX]]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node13_reduce_node5_$$plus_9 [NX] : Reduce[Plus.type, Node13[Node9[NX]], Node5[Node9[NX]]] = Reduce(s => Node5(s.prev, s.value))
  implicit def node13_reduce_node0_$$plus_14 : Reduce[Plus.type, Node13[Node14.type], Node0[Node14.type]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node13_reduce_node0_end_6 [NX] : Reduce[End.type, Node13[Node6[NX]], Node0[Node6[NX]]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node13_reduce_node5_end_9 [NX] : Reduce[End.type, Node13[Node9[NX]], Node5[Node9[NX]]] = Reduce(s => Node5(s.prev, s.value))
  implicit def node13_reduce_node0_end_14 : Reduce[End.type, Node13[Node14.type], Node0[Node14.type]] = Reduce(s => Node0(s.prev, s.value))
  implicit def node15_reduce_node11_end_6_0_3 [NX] : Reduce[End.type, Node15[Node3[Node0[Node6[NX]]]], Node11[Node6[NX]]] = Reduce(s => Node11(s.prev.prev.prev, Add(s.prev.prev.value, s.value)))
  implicit def node15_reduce_node11_end_14_0_3 : Reduce[End.type, Node15[Node3[Node0[Node14.type]]], Node11[Node14.type]] = Reduce(s => Node11(s.prev.prev.prev, Add(s.prev.prev.value, s.value)))
  implicit def node15_reduce_node11_end_9_5_3 [NX] : Reduce[End.type, Node15[Node3[Node5[Node9[NX]]]], Node11[Node9[NX]]] = Reduce(s => Node11(s.prev.prev.prev, Add(s.prev.prev.value, s.value)))
  implicit def node15_reduce_node11_$$hyphen_6_0_3 [NX] : Reduce[Minus.type, Node15[Node3[Node0[Node6[NX]]]], Node11[Node6[NX]]] = Reduce(s => Node11(s.prev.prev.prev, Add(s.prev.prev.value, s.value)))
  implicit def node15_reduce_node11_$$hyphen_14_0_3 : Reduce[Minus.type, Node15[Node3[Node0[Node14.type]]], Node11[Node14.type]] = Reduce(s => Node11(s.prev.prev.prev, Add(s.prev.prev.value, s.value)))
  implicit def node15_reduce_node11_$$hyphen_9_5_3 [NX] : Reduce[Minus.type, Node15[Node3[Node5[Node9[NX]]]], Node11[Node9[NX]]] = Reduce(s => Node11(s.prev.prev.prev, Add(s.prev.prev.value, s.value)))
  implicit def node15_reduce_node11_$$plus_6_0_3 [NX] : Reduce[Plus.type, Node15[Node3[Node0[Node6[NX]]]], Node11[Node6[NX]]] = Reduce(s => Node11(s.prev.prev.prev, Add(s.prev.prev.value, s.value)))
  implicit def node15_reduce_node11_$$plus_14_0_3 : Reduce[Plus.type, Node15[Node3[Node0[Node14.type]]], Node11[Node14.type]] = Reduce(s => Node11(s.prev.prev.prev, Add(s.prev.prev.value, s.value)))
  implicit def node15_reduce_node11_$$plus_9_5_3 [NX] : Reduce[Plus.type, Node15[Node3[Node5[Node9[NX]]]], Node11[Node9[NX]]] = Reduce(s => Node11(s.prev.prev.prev, Add(s.prev.prev.value, s.value)))
  implicit def node15_reduce_node11_$$parenright_6_0_3 [NX] : Reduce[Rp.type, Node15[Node3[Node0[Node6[NX]]]], Node11[Node6[NX]]] = Reduce(s => Node11(s.prev.prev.prev, Add(s.prev.prev.value, s.value)))
  implicit def node15_reduce_node11_$$parenright_14_0_3 : Reduce[Rp.type, Node15[Node3[Node0[Node14.type]]], Node11[Node14.type]] = Reduce(s => Node11(s.prev.prev.prev, Add(s.prev.prev.value, s.value)))
  implicit def node15_reduce_node11_$$parenright_9_5_3 [NX] : Reduce[Rp.type, Node15[Node3[Node5[Node9[NX]]]], Node11[Node9[NX]]] = Reduce(s => Node11(s.prev.prev.prev, Add(s.prev.prev.value, s.value)))
  implicit def node16_reduce_node4_eoi_6_6 [NX] : Reduce[EoI.type, Node16[Node6[Node6[NX]]], Node4[Node6[NX]]] = Reduce(s => Node4(s.prev.prev, Stmts(s.prev.value, s.value)))
  implicit def node16_reduce_node4_eoi_14_6 : Reduce[EoI.type, Node16[Node6[Node14.type]], Node4[Node14.type]] = Reduce(s => Node4(s.prev.prev, Stmts(s.prev.value, s.value)))
  implicit def node17_reduce_node7_$$asterisk_9 [NX] : Reduce[Mul.type, Node17[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node17_reduce_node18_$$asterisk_19 [NX] : Reduce[Mul.type, Node17[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node17_reduce_node7_$$asterisk_6 [NX] : Reduce[Mul.type, Node17[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node17_reduce_node15_$$asterisk_3 [NX] : Reduce[Mul.type, Node17[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node17_reduce_node7_$$asterisk_14 : Reduce[Mul.type, Node17[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node17_reduce_node7_$$parenright_6 [NX] : Reduce[Rp.type, Node17[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node17_reduce_node18_$$parenright_19 [NX] : Reduce[Rp.type, Node17[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node17_reduce_node15_$$parenright_3 [NX] : Reduce[Rp.type, Node17[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node17_reduce_node7_$$parenright_9 [NX] : Reduce[Rp.type, Node17[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node17_reduce_node7_$$parenright_14 : Reduce[Rp.type, Node17[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node17_reduce_node7_$$hyphen_14 : Reduce[Minus.type, Node17[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node17_reduce_node18_$$hyphen_19 [NX] : Reduce[Minus.type, Node17[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node17_reduce_node15_$$hyphen_3 [NX] : Reduce[Minus.type, Node17[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node17_reduce_node7_$$hyphen_9 [NX] : Reduce[Minus.type, Node17[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node17_reduce_node7_$$hyphen_6 [NX] : Reduce[Minus.type, Node17[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node17_reduce_node7_end_9 [NX] : Reduce[End.type, Node17[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node17_reduce_node15_end_3 [NX] : Reduce[End.type, Node17[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node17_reduce_node7_end_14 : Reduce[End.type, Node17[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node17_reduce_node18_end_19 [NX] : Reduce[End.type, Node17[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node17_reduce_node7_end_6 [NX] : Reduce[End.type, Node17[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node17_reduce_node15_$$plus_3 [NX] : Reduce[Plus.type, Node17[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node17_reduce_node7_$$plus_9 [NX] : Reduce[Plus.type, Node17[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node17_reduce_node7_$$plus_14 : Reduce[Plus.type, Node17[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node17_reduce_node7_$$plus_6 [NX] : Reduce[Plus.type, Node17[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node17_reduce_node18_$$plus_19 [NX] : Reduce[Plus.type, Node17[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node17_reduce_node7_$$slash_9 [NX] : Reduce[Div.type, Node17[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node17_reduce_node18_$$slash_19 [NX] : Reduce[Div.type, Node17[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node17_reduce_node7_$$slash_14 : Reduce[Div.type, Node17[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node17_reduce_node15_$$slash_3 [NX] : Reduce[Div.type, Node17[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node17_reduce_node7_$$slash_6 [NX] : Reduce[Div.type, Node17[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node18_reduce_node13_end_6_0_19 [NX] : Reduce[End.type, Node18[Node19[Node0[Node6[NX]]]], Node13[Node6[NX]]] = Reduce(s => Node13(s.prev.prev.prev, Sub(s.prev.prev.value, s.value)))
  implicit def node18_reduce_node13_end_14_0_19 : Reduce[End.type, Node18[Node19[Node0[Node14.type]]], Node13[Node14.type]] = Reduce(s => Node13(s.prev.prev.prev, Sub(s.prev.prev.value, s.value)))
  implicit def node18_reduce_node13_end_9_5_19 [NX] : Reduce[End.type, Node18[Node19[Node5[Node9[NX]]]], Node13[Node9[NX]]] = Reduce(s => Node13(s.prev.prev.prev, Sub(s.prev.prev.value, s.value)))
  implicit def node18_reduce_node13_$$hyphen_6_0_19 [NX] : Reduce[Minus.type, Node18[Node19[Node0[Node6[NX]]]], Node13[Node6[NX]]] = Reduce(s => Node13(s.prev.prev.prev, Sub(s.prev.prev.value, s.value)))
  implicit def node18_reduce_node13_$$hyphen_14_0_19 : Reduce[Minus.type, Node18[Node19[Node0[Node14.type]]], Node13[Node14.type]] = Reduce(s => Node13(s.prev.prev.prev, Sub(s.prev.prev.value, s.value)))
  implicit def node18_reduce_node13_$$hyphen_9_5_19 [NX] : Reduce[Minus.type, Node18[Node19[Node5[Node9[NX]]]], Node13[Node9[NX]]] = Reduce(s => Node13(s.prev.prev.prev, Sub(s.prev.prev.value, s.value)))
  implicit def node18_reduce_node13_$$plus_6_0_19 [NX] : Reduce[Plus.type, Node18[Node19[Node0[Node6[NX]]]], Node13[Node6[NX]]] = Reduce(s => Node13(s.prev.prev.prev, Sub(s.prev.prev.value, s.value)))
  implicit def node18_reduce_node13_$$plus_14_0_19 : Reduce[Plus.type, Node18[Node19[Node0[Node14.type]]], Node13[Node14.type]] = Reduce(s => Node13(s.prev.prev.prev, Sub(s.prev.prev.value, s.value)))
  implicit def node18_reduce_node13_$$plus_9_5_19 [NX] : Reduce[Plus.type, Node18[Node19[Node5[Node9[NX]]]], Node13[Node9[NX]]] = Reduce(s => Node13(s.prev.prev.prev, Sub(s.prev.prev.value, s.value)))
  implicit def node18_reduce_node13_$$parenright_6_0_19 [NX] : Reduce[Rp.type, Node18[Node19[Node0[Node6[NX]]]], Node13[Node6[NX]]] = Reduce(s => Node13(s.prev.prev.prev, Sub(s.prev.prev.value, s.value)))
  implicit def node18_reduce_node13_$$parenright_14_0_19 : Reduce[Rp.type, Node18[Node19[Node0[Node14.type]]], Node13[Node14.type]] = Reduce(s => Node13(s.prev.prev.prev, Sub(s.prev.prev.value, s.value)))
  implicit def node18_reduce_node13_$$parenright_9_5_19 [NX] : Reduce[Rp.type, Node18[Node19[Node5[Node9[NX]]]], Node13[Node9[NX]]] = Reduce(s => Node13(s.prev.prev.prev, Sub(s.prev.prev.value, s.value)))
  implicit def node20_reduce_node7_$$asterisk_14 : Reduce[Mul.type, Node20[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node15_$$asterisk_3 [NX] : Reduce[Mul.type, Node20[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node20_reduce_node7_$$asterisk_6 [NX] : Reduce[Mul.type, Node20[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node18_$$asterisk_19 [NX] : Reduce[Mul.type, Node20[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node20_reduce_node7_$$asterisk_9 [NX] : Reduce[Mul.type, Node20[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node15_$$parenright_3 [NX] : Reduce[Rp.type, Node20[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node20_reduce_node7_$$parenright_9 [NX] : Reduce[Rp.type, Node20[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node7_$$parenright_6 [NX] : Reduce[Rp.type, Node20[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node7_$$parenright_14 : Reduce[Rp.type, Node20[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node18_$$parenright_19 [NX] : Reduce[Rp.type, Node20[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node20_reduce_node7_$$hyphen_14 : Reduce[Minus.type, Node20[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node7_$$hyphen_6 [NX] : Reduce[Minus.type, Node20[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node18_$$hyphen_19 [NX] : Reduce[Minus.type, Node20[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node20_reduce_node7_$$hyphen_9 [NX] : Reduce[Minus.type, Node20[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node15_$$hyphen_3 [NX] : Reduce[Minus.type, Node20[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node20_reduce_node18_end_19 [NX] : Reduce[End.type, Node20[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node20_reduce_node7_end_6 [NX] : Reduce[End.type, Node20[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node7_end_14 : Reduce[End.type, Node20[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node7_end_9 [NX] : Reduce[End.type, Node20[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node15_end_3 [NX] : Reduce[End.type, Node20[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node20_reduce_node7_$$plus_6 [NX] : Reduce[Plus.type, Node20[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node18_$$plus_19 [NX] : Reduce[Plus.type, Node20[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node20_reduce_node15_$$plus_3 [NX] : Reduce[Plus.type, Node20[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node20_reduce_node7_$$plus_14 : Reduce[Plus.type, Node20[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node7_$$plus_9 [NX] : Reduce[Plus.type, Node20[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node7_$$slash_9 [NX] : Reduce[Div.type, Node20[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node18_$$slash_19 [NX] : Reduce[Div.type, Node20[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node20_reduce_node7_$$slash_14 : Reduce[Div.type, Node20[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node7_$$slash_6 [NX] : Reduce[Div.type, Node20[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node20_reduce_node15_$$slash_3 [NX] : Reduce[Div.type, Node20[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node21_reduce_node23_$$asterisk_14 : Reduce[Mul.type, Node21[Node14.type], Node23[Node14.type]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_$$asterisk_3 [NX] : Reduce[Mul.type, Node21[Node3[NX]], Node23[Node3[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node12_$$asterisk_26 [NX] : Reduce[Mul.type, Node21[Node26[NX]], Node12[Node26[NX]]] = Reduce(s => Node12(s.prev, s.value))
  implicit def node21_reduce_node23_$$asterisk_19 [NX] : Reduce[Mul.type, Node21[Node19[NX]], Node23[Node19[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node25_$$asterisk_22 [NX] : Reduce[Mul.type, Node21[Node22[NX]], Node25[Node22[NX]]] = Reduce(s => Node25(s.prev, s.value))
  implicit def node21_reduce_node23_$$asterisk_9 [NX] : Reduce[Mul.type, Node21[Node9[NX]], Node23[Node9[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_$$asterisk_6 [NX] : Reduce[Mul.type, Node21[Node6[NX]], Node23[Node6[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node25_$$parenright_22 [NX] : Reduce[Rp.type, Node21[Node22[NX]], Node25[Node22[NX]]] = Reduce(s => Node25(s.prev, s.value))
  implicit def node21_reduce_node23_$$parenright_3 [NX] : Reduce[Rp.type, Node21[Node3[NX]], Node23[Node3[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_$$parenright_19 [NX] : Reduce[Rp.type, Node21[Node19[NX]], Node23[Node19[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_$$parenright_14 : Reduce[Rp.type, Node21[Node14.type], Node23[Node14.type]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_$$parenright_6 [NX] : Reduce[Rp.type, Node21[Node6[NX]], Node23[Node6[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node12_$$parenright_26 [NX] : Reduce[Rp.type, Node21[Node26[NX]], Node12[Node26[NX]]] = Reduce(s => Node12(s.prev, s.value))
  implicit def node21_reduce_node23_$$parenright_9 [NX] : Reduce[Rp.type, Node21[Node9[NX]], Node23[Node9[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node12_$$hyphen_26 [NX] : Reduce[Minus.type, Node21[Node26[NX]], Node12[Node26[NX]]] = Reduce(s => Node12(s.prev, s.value))
  implicit def node21_reduce_node25_$$hyphen_22 [NX] : Reduce[Minus.type, Node21[Node22[NX]], Node25[Node22[NX]]] = Reduce(s => Node25(s.prev, s.value))
  implicit def node21_reduce_node23_$$hyphen_19 [NX] : Reduce[Minus.type, Node21[Node19[NX]], Node23[Node19[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_$$hyphen_9 [NX] : Reduce[Minus.type, Node21[Node9[NX]], Node23[Node9[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_$$hyphen_3 [NX] : Reduce[Minus.type, Node21[Node3[NX]], Node23[Node3[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_$$hyphen_6 [NX] : Reduce[Minus.type, Node21[Node6[NX]], Node23[Node6[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_$$hyphen_14 : Reduce[Minus.type, Node21[Node14.type], Node23[Node14.type]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node25_end_22 [NX] : Reduce[End.type, Node21[Node22[NX]], Node25[Node22[NX]]] = Reduce(s => Node25(s.prev, s.value))
  implicit def node21_reduce_node23_end_6 [NX] : Reduce[End.type, Node21[Node6[NX]], Node23[Node6[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node12_end_26 [NX] : Reduce[End.type, Node21[Node26[NX]], Node12[Node26[NX]]] = Reduce(s => Node12(s.prev, s.value))
  implicit def node21_reduce_node23_end_3 [NX] : Reduce[End.type, Node21[Node3[NX]], Node23[Node3[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_end_19 [NX] : Reduce[End.type, Node21[Node19[NX]], Node23[Node19[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_end_14 : Reduce[End.type, Node21[Node14.type], Node23[Node14.type]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_end_9 [NX] : Reduce[End.type, Node21[Node9[NX]], Node23[Node9[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_$$plus_19 [NX] : Reduce[Plus.type, Node21[Node19[NX]], Node23[Node19[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node12_$$plus_26 [NX] : Reduce[Plus.type, Node21[Node26[NX]], Node12[Node26[NX]]] = Reduce(s => Node12(s.prev, s.value))
  implicit def node21_reduce_node23_$$plus_6 [NX] : Reduce[Plus.type, Node21[Node6[NX]], Node23[Node6[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_$$plus_9 [NX] : Reduce[Plus.type, Node21[Node9[NX]], Node23[Node9[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_$$plus_14 : Reduce[Plus.type, Node21[Node14.type], Node23[Node14.type]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_$$plus_3 [NX] : Reduce[Plus.type, Node21[Node3[NX]], Node23[Node3[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node25_$$plus_22 [NX] : Reduce[Plus.type, Node21[Node22[NX]], Node25[Node22[NX]]] = Reduce(s => Node25(s.prev, s.value))
  implicit def node21_reduce_node23_$$slash_14 : Reduce[Div.type, Node21[Node14.type], Node23[Node14.type]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_$$slash_9 [NX] : Reduce[Div.type, Node21[Node9[NX]], Node23[Node9[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_$$slash_6 [NX] : Reduce[Div.type, Node21[Node6[NX]], Node23[Node6[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node23_$$slash_19 [NX] : Reduce[Div.type, Node21[Node19[NX]], Node23[Node19[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node25_$$slash_22 [NX] : Reduce[Div.type, Node21[Node22[NX]], Node25[Node22[NX]]] = Reduce(s => Node25(s.prev, s.value))
  implicit def node21_reduce_node23_$$slash_3 [NX] : Reduce[Div.type, Node21[Node3[NX]], Node23[Node3[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node21_reduce_node12_$$slash_26 [NX] : Reduce[Div.type, Node21[Node26[NX]], Node12[Node26[NX]]] = Reduce(s => Node12(s.prev, s.value))
  implicit def node23_reduce_node18_$$asterisk_19 [NX] : Reduce[Mul.type, Node23[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node23_reduce_node7_$$asterisk_14 : Reduce[Mul.type, Node23[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node23_reduce_node7_$$asterisk_6 [NX] : Reduce[Mul.type, Node23[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node23_reduce_node15_$$asterisk_3 [NX] : Reduce[Mul.type, Node23[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node23_reduce_node7_$$asterisk_9 [NX] : Reduce[Mul.type, Node23[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node23_reduce_node7_$$parenright_14 : Reduce[Rp.type, Node23[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node23_reduce_node15_$$parenright_3 [NX] : Reduce[Rp.type, Node23[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node23_reduce_node18_$$parenright_19 [NX] : Reduce[Rp.type, Node23[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node23_reduce_node7_$$parenright_9 [NX] : Reduce[Rp.type, Node23[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node23_reduce_node7_$$parenright_6 [NX] : Reduce[Rp.type, Node23[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node23_reduce_node18_$$hyphen_19 [NX] : Reduce[Minus.type, Node23[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node23_reduce_node7_$$hyphen_6 [NX] : Reduce[Minus.type, Node23[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node23_reduce_node15_$$hyphen_3 [NX] : Reduce[Minus.type, Node23[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node23_reduce_node7_$$hyphen_9 [NX] : Reduce[Minus.type, Node23[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node23_reduce_node7_$$hyphen_14 : Reduce[Minus.type, Node23[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node23_reduce_node7_end_6 [NX] : Reduce[End.type, Node23[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node23_reduce_node7_end_9 [NX] : Reduce[End.type, Node23[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node23_reduce_node15_end_3 [NX] : Reduce[End.type, Node23[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node23_reduce_node7_end_14 : Reduce[End.type, Node23[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node23_reduce_node18_end_19 [NX] : Reduce[End.type, Node23[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node23_reduce_node7_$$plus_14 : Reduce[Plus.type, Node23[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node23_reduce_node7_$$plus_6 [NX] : Reduce[Plus.type, Node23[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node23_reduce_node18_$$plus_19 [NX] : Reduce[Plus.type, Node23[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node23_reduce_node15_$$plus_3 [NX] : Reduce[Plus.type, Node23[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node23_reduce_node7_$$plus_9 [NX] : Reduce[Plus.type, Node23[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node23_reduce_node18_$$slash_19 [NX] : Reduce[Div.type, Node23[Node19[NX]], Node18[Node19[NX]]] = Reduce(s => Node18(s.prev, s.value))
  implicit def node23_reduce_node7_$$slash_6 [NX] : Reduce[Div.type, Node23[Node6[NX]], Node7[Node6[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node23_reduce_node7_$$slash_14 : Reduce[Div.type, Node23[Node14.type], Node7[Node14.type]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node23_reduce_node15_$$slash_3 [NX] : Reduce[Div.type, Node23[Node3[NX]], Node15[Node3[NX]]] = Reduce(s => Node15(s.prev, s.value))
  implicit def node23_reduce_node7_$$slash_9 [NX] : Reduce[Div.type, Node23[Node9[NX]], Node7[Node9[NX]]] = Reduce(s => Node7(s.prev, s.value))
  implicit def node24_reduce_node23_$$asterisk_3 [NX] : Reduce[Mul.type, Node24[Node3[NX]], Node23[Node3[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node25_$$asterisk_22 [NX] : Reduce[Mul.type, Node24[Node22[NX]], Node25[Node22[NX]]] = Reduce(s => Node25(s.prev, s.value))
  implicit def node24_reduce_node23_$$asterisk_19 [NX] : Reduce[Mul.type, Node24[Node19[NX]], Node23[Node19[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_$$asterisk_6 [NX] : Reduce[Mul.type, Node24[Node6[NX]], Node23[Node6[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_$$asterisk_9 [NX] : Reduce[Mul.type, Node24[Node9[NX]], Node23[Node9[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node12_$$asterisk_26 [NX] : Reduce[Mul.type, Node24[Node26[NX]], Node12[Node26[NX]]] = Reduce(s => Node12(s.prev, s.value))
  implicit def node24_reduce_node23_$$asterisk_14 : Reduce[Mul.type, Node24[Node14.type], Node23[Node14.type]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_$$parenright_6 [NX] : Reduce[Rp.type, Node24[Node6[NX]], Node23[Node6[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node12_$$parenright_26 [NX] : Reduce[Rp.type, Node24[Node26[NX]], Node12[Node26[NX]]] = Reduce(s => Node12(s.prev, s.value))
  implicit def node24_reduce_node23_$$parenright_19 [NX] : Reduce[Rp.type, Node24[Node19[NX]], Node23[Node19[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_$$parenright_14 : Reduce[Rp.type, Node24[Node14.type], Node23[Node14.type]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_$$parenright_3 [NX] : Reduce[Rp.type, Node24[Node3[NX]], Node23[Node3[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_$$parenright_9 [NX] : Reduce[Rp.type, Node24[Node9[NX]], Node23[Node9[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node25_$$parenright_22 [NX] : Reduce[Rp.type, Node24[Node22[NX]], Node25[Node22[NX]]] = Reduce(s => Node25(s.prev, s.value))
  implicit def node24_reduce_node25_$$hyphen_22 [NX] : Reduce[Minus.type, Node24[Node22[NX]], Node25[Node22[NX]]] = Reduce(s => Node25(s.prev, s.value))
  implicit def node24_reduce_node23_$$hyphen_3 [NX] : Reduce[Minus.type, Node24[Node3[NX]], Node23[Node3[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_$$hyphen_6 [NX] : Reduce[Minus.type, Node24[Node6[NX]], Node23[Node6[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_$$hyphen_19 [NX] : Reduce[Minus.type, Node24[Node19[NX]], Node23[Node19[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_$$hyphen_9 [NX] : Reduce[Minus.type, Node24[Node9[NX]], Node23[Node9[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node12_$$hyphen_26 [NX] : Reduce[Minus.type, Node24[Node26[NX]], Node12[Node26[NX]]] = Reduce(s => Node12(s.prev, s.value))
  implicit def node24_reduce_node23_$$hyphen_14 : Reduce[Minus.type, Node24[Node14.type], Node23[Node14.type]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_end_3 [NX] : Reduce[End.type, Node24[Node3[NX]], Node23[Node3[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node25_end_22 [NX] : Reduce[End.type, Node24[Node22[NX]], Node25[Node22[NX]]] = Reduce(s => Node25(s.prev, s.value))
  implicit def node24_reduce_node23_end_19 [NX] : Reduce[End.type, Node24[Node19[NX]], Node23[Node19[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_end_6 [NX] : Reduce[End.type, Node24[Node6[NX]], Node23[Node6[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node12_end_26 [NX] : Reduce[End.type, Node24[Node26[NX]], Node12[Node26[NX]]] = Reduce(s => Node12(s.prev, s.value))
  implicit def node24_reduce_node23_end_9 [NX] : Reduce[End.type, Node24[Node9[NX]], Node23[Node9[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_end_14 : Reduce[End.type, Node24[Node14.type], Node23[Node14.type]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_$$plus_14 : Reduce[Plus.type, Node24[Node14.type], Node23[Node14.type]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node25_$$plus_22 [NX] : Reduce[Plus.type, Node24[Node22[NX]], Node25[Node22[NX]]] = Reduce(s => Node25(s.prev, s.value))
  implicit def node24_reduce_node23_$$plus_6 [NX] : Reduce[Plus.type, Node24[Node6[NX]], Node23[Node6[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_$$plus_9 [NX] : Reduce[Plus.type, Node24[Node9[NX]], Node23[Node9[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_$$plus_19 [NX] : Reduce[Plus.type, Node24[Node19[NX]], Node23[Node19[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_$$plus_3 [NX] : Reduce[Plus.type, Node24[Node3[NX]], Node23[Node3[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node12_$$plus_26 [NX] : Reduce[Plus.type, Node24[Node26[NX]], Node12[Node26[NX]]] = Reduce(s => Node12(s.prev, s.value))
  implicit def node24_reduce_node12_$$slash_26 [NX] : Reduce[Div.type, Node24[Node26[NX]], Node12[Node26[NX]]] = Reduce(s => Node12(s.prev, s.value))
  implicit def node24_reduce_node25_$$slash_22 [NX] : Reduce[Div.type, Node24[Node22[NX]], Node25[Node22[NX]]] = Reduce(s => Node25(s.prev, s.value))
  implicit def node24_reduce_node23_$$slash_9 [NX] : Reduce[Div.type, Node24[Node9[NX]], Node23[Node9[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_$$slash_14 : Reduce[Div.type, Node24[Node14.type], Node23[Node14.type]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_$$slash_6 [NX] : Reduce[Div.type, Node24[Node6[NX]], Node23[Node6[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_$$slash_19 [NX] : Reduce[Div.type, Node24[Node19[NX]], Node23[Node19[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node24_reduce_node23_$$slash_3 [NX] : Reduce[Div.type, Node24[Node3[NX]], Node23[Node3[NX]]] = Reduce(s => Node23(s.prev, s.value))
  implicit def node25_reduce_node17_$$asterisk_9_7_22 [NX] : Reduce[Mul.type, Node25[Node22[Node7[Node9[NX]]]], Node17[Node9[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$asterisk_6_7_22 [NX] : Reduce[Mul.type, Node25[Node22[Node7[Node6[NX]]]], Node17[Node6[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$asterisk_19_18_22 [NX] : Reduce[Mul.type, Node25[Node22[Node18[Node19[NX]]]], Node17[Node19[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$asterisk_3_15_22 [NX] : Reduce[Mul.type, Node25[Node22[Node15[Node3[NX]]]], Node17[Node3[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$asterisk_14_7_22 : Reduce[Mul.type, Node25[Node22[Node7[Node14.type]]], Node17[Node14.type]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$parenright_6_7_22 [NX] : Reduce[Rp.type, Node25[Node22[Node7[Node6[NX]]]], Node17[Node6[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$parenright_9_7_22 [NX] : Reduce[Rp.type, Node25[Node22[Node7[Node9[NX]]]], Node17[Node9[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$parenright_14_7_22 : Reduce[Rp.type, Node25[Node22[Node7[Node14.type]]], Node17[Node14.type]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$parenright_19_18_22 [NX] : Reduce[Rp.type, Node25[Node22[Node18[Node19[NX]]]], Node17[Node19[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$parenright_3_15_22 [NX] : Reduce[Rp.type, Node25[Node22[Node15[Node3[NX]]]], Node17[Node3[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$hyphen_6_7_22 [NX] : Reduce[Minus.type, Node25[Node22[Node7[Node6[NX]]]], Node17[Node6[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$hyphen_19_18_22 [NX] : Reduce[Minus.type, Node25[Node22[Node18[Node19[NX]]]], Node17[Node19[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$hyphen_14_7_22 : Reduce[Minus.type, Node25[Node22[Node7[Node14.type]]], Node17[Node14.type]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$hyphen_9_7_22 [NX] : Reduce[Minus.type, Node25[Node22[Node7[Node9[NX]]]], Node17[Node9[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$hyphen_3_15_22 [NX] : Reduce[Minus.type, Node25[Node22[Node15[Node3[NX]]]], Node17[Node3[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_end_6_7_22 [NX] : Reduce[End.type, Node25[Node22[Node7[Node6[NX]]]], Node17[Node6[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_end_9_7_22 [NX] : Reduce[End.type, Node25[Node22[Node7[Node9[NX]]]], Node17[Node9[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_end_14_7_22 : Reduce[End.type, Node25[Node22[Node7[Node14.type]]], Node17[Node14.type]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_end_19_18_22 [NX] : Reduce[End.type, Node25[Node22[Node18[Node19[NX]]]], Node17[Node19[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_end_3_15_22 [NX] : Reduce[End.type, Node25[Node22[Node15[Node3[NX]]]], Node17[Node3[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$plus_3_15_22 [NX] : Reduce[Plus.type, Node25[Node22[Node15[Node3[NX]]]], Node17[Node3[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$plus_14_7_22 : Reduce[Plus.type, Node25[Node22[Node7[Node14.type]]], Node17[Node14.type]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$plus_6_7_22 [NX] : Reduce[Plus.type, Node25[Node22[Node7[Node6[NX]]]], Node17[Node6[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$plus_9_7_22 [NX] : Reduce[Plus.type, Node25[Node22[Node7[Node9[NX]]]], Node17[Node9[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$plus_19_18_22 [NX] : Reduce[Plus.type, Node25[Node22[Node18[Node19[NX]]]], Node17[Node19[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$slash_3_15_22 [NX] : Reduce[Div.type, Node25[Node22[Node15[Node3[NX]]]], Node17[Node3[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$slash_19_18_22 [NX] : Reduce[Div.type, Node25[Node22[Node18[Node19[NX]]]], Node17[Node19[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$slash_9_7_22 [NX] : Reduce[Div.type, Node25[Node22[Node7[Node9[NX]]]], Node17[Node9[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$slash_14_7_22 : Reduce[Div.type, Node25[Node22[Node7[Node14.type]]], Node17[Node14.type]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))
  implicit def node25_reduce_node17_$$slash_6_7_22 [NX] : Reduce[Div.type, Node25[Node22[Node7[Node6[NX]]]], Node17[Node6[NX]]] = Reduce(s => Node17(s.prev.prev.prev, Div(s.prev.prev.value, s.value)))

}

object Main2 {
  import MathDSL2._

  def main (args: Array[String]): Unit = {

    val program: Program = int(10) $$plus int(2) $$asterisk $$parenleft int (10) $$slash int(5) $$parenright end
    println(program)

    val program2: Program = int(10).$$plus.int(2).$$asterisk.$$parenleft.int(10).$$slash.int(5).$$parenright.end
    println(program2)

    val program3: Program = (10) $$plus (2) $$asterisk $$parenleft (10) $$slash (5) $$parenright end
    println(program3)

    val program4: Program = 10 $$plus 2 $$asterisk $$parenleft (10) $$slash 5 $$parenright end
    println(program4)

    val program5: Program = (10).$$plus(2).$$asterisk.$$parenleft(10).$$slash(5).$$parenright.end
    println(program5)

    val program6: Program = $$parenleft (10) $$plus (2) $$parenright $$asterisk $$parenleft $$parenleft $$parenleft (10) $$slash (5) $$parenright $$parenright $$parenright end
    println(program6)

    val program7: Program = $$parenleft (10) $$plus (2) $$parenright $$asterisk $$parenleft $$parenleft (10) $$slash (5) $$parenright $$parenright end $$semicolon
    println(program7)

    /**/
  }
}
