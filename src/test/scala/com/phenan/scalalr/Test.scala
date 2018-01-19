package com.phenan.scalalr

@dsl
object Test {
  @start
  @syntax
  sealed trait Hoge

  @syntax(g"foo $n with $m")
  case class Foo (n: Int, m: Bar) extends Hoge

  @syntax(g"bar $s bar bar")
  case class Bar (s: List[String]) extends Hoge

  @syntax(g"baz")
  case object Baz extends Hoge

  @token
  def int (n: Int): Int = n

  @token
  def string (s: String): String = s

}
