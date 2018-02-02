package com.phenan.scalalr

import org.scalatest._

import scala.language.postfixOps

class MathDSLTest extends FunSuite with DiagrammedAssertions {
  import MathDSL._

  test ("test simple expression") {
    val x: Expr = (1) $$plus (2)

    assert (x == Add(Num(1), Num(2)))
  }

  test ("test complex expression") {
    val x: Expr = $$parenleft $$parenleft (1) $$plus (2) $$slash (3) $$parenright $$asterisk (4) $$parenright

    assert (x == Paren(Mul(Paren(Add(Num(1), Div(Num(2), Num(3)))), Num(4))))
  }

  test ("test AST interpolation") {
    val x: Expr = $$parenleft $$parenleft (1) $$hyphen (Mul(Num(2), Num(2))) $$slash (3) $$parenright $$asterisk (4) $$parenright

    assert (x == Paren(Mul(Paren(Sub(Num(1), Div(Mul(Num(2), Num(2)), Num(3)))), Num(4))))
  }

  test ("test ints") {
    val x : Expr = $$braceleft $$braceright

    println(x)

    val y : Expr = $$braceleft (1) $$braceright

    println(y)

    val z : Expr = $$braceleft (1)$$comma (2) $$braceright

    println(z)
  }
}
