package com.phenan.scalalr

@dsl[MathDSL.Expr]
object MathDSL {

  sealed trait Expr

  @syntax(s"$n + $m")
  case class Add (n: Expr, m: Term) extends Expr

  @syntax(s"$n - $m")
  case class Sub (n: Expr, m: Term) extends Expr

  sealed trait Term extends Expr

  @syntax(s"$n * $m")
  case class Mul (n: Term, m: Factor) extends Term

  @syntax(s"$n / $m")
  case class Div (n: Term, m: Factor) extends Term

  sealed trait Factor extends Term

  @syntax(s"$n")
  case class Num (n: Int) extends Factor

  @syntax(s"( $e )")
  case class Paren (e: Expr) extends Factor

  @syntax(s"{ $ns }")
  case class Ints (ns: Int@sep(",")*) extends Expr
}
