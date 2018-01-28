package com.phenan.scalalr

@dsl[Test.Foo]
object Test {
  @syntax
  sealed trait Hoge

  @syntax(g"foo $n with $m")
  case class Foo (n: Int, m: Bar) extends Hoge

  @syntax(g"bar $s bar bar")
  case class Bar (s: String) extends Hoge

  @syntax(g"baz")
  case object Baz extends Hoge

  //@syntax(g"foo $n bar $s")
  //def foo (n: Int, s: String): List[scala.Unit] @syntax(g"foo $n bar $s") = ???

  //val bar: Nil.type = Nil
  //val baz: Test.Baz.type = Baz
  //val hoge: Int = 0
}

/*@dsl[Unit]
object Test2 {
  @syntax(g"foo $n bar $s")
  def foo (n: Int, s: String): List[scala.Unit] @syntax(g"foo $n bar $s") = ???
}*/









