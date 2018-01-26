package com.phenan.arith

sealed trait Expr extends Stmt
case class Mul  (arg0: Term, arg1: Factor) extends Term
sealed trait Term extends Expr
case class Stmts  (arg0: Stmt, arg1: Program) extends Program
case class Div  (arg0: Term, arg1: Factor) extends Term
case class Num  (arg0: Int) extends Factor
sealed trait Factor extends Term
sealed trait Stmt extends Program
case class Paren  (arg0: Expr) extends Factor
sealed trait Program
case class Add  (arg0: Expr, arg1: Term) extends Expr
case class Sub  (arg0: Expr, arg1: Term) extends Expr
