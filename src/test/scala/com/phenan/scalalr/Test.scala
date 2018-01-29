package com.phenan.scalalr

@dsl[Test.Expr]
object Test {

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

  def main (args: Array[String]): Unit = {

    val x: Expr = $$parenleft $$parenleft (1) $$plus (2) $$slash (3) $$parenright $$asterisk (4) $$parenright

    println(x)

    val y: Expr = $$parenleft $$parenleft (1) $$hyphen (Mul(Num(2), Num(2))) $$slash (3) $$parenright $$asterisk (4) $$parenright

    println(y)
  }
}
