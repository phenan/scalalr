package com.phenan.scalalr.macroimpl

trait SyntaxInfoModule {
  this: MacroModule =>

  import c.universe._

  case class SyntaxInfo (returnType: Tree, operators: List[List[String]], operandTypes: List[Tree], semantics: SemanticActionImpl)

  object SyntaxInfo {
    def epsilonOperator (returnType: Tree, returnValue: Tree): SyntaxInfo = {
      SyntaxInfo(returnType, Nil, Nil, SemanticActionImpl.returnConstant(returnValue))
    }

    def unaryOperator (returnType: Tree, prefix: List[String], operandType: Tree, postfix: List[String], semantics: Tree => Tree): SyntaxInfo = {
      SyntaxInfo(returnType, List(prefix, postfix), List(operandType), SemanticActionImpl.unaryOperation(semantics))
    }

    def binaryOperator (returnType: Tree, prefix: List[String], operandType1: Tree, infix: List[String], operandType2: Tree, postfix: List[String], semantics: (Tree, Tree) => Tree): SyntaxInfo = {
      SyntaxInfo(returnType, List(prefix, infix, postfix), List(operandType1, operandType2), SemanticActionImpl.binaryOperation(semantics))
    }
  }

  class SemanticActionImpl (val run: List[Tree] => Tree)

  object SemanticActionImpl {

    lazy val returnArgument: SemanticActionImpl = SemanticActionImpl { args =>
      if (args.lengthCompare(1) == 0) args.head
      else c.abort(c.enclosingPosition, s"wrong macro implementation: expected one argument, but takes ${args.mkString("(", ", ", ")")}")
    }

    def returnConstant (constant: Tree): SemanticActionImpl = SemanticActionImpl { args =>
      if (args.isEmpty) constant
      else c.abort(c.enclosingPosition, s"wrong macro implementation: expected no argument, but takes ${args.mkString("(", ", ", ")")}")
    }

    def unaryOperation (operator: Tree => Tree): SemanticActionImpl = SemanticActionImpl { args =>
      if (args.lengthCompare(1) == 0) operator(args.head)
      else c.abort(c.enclosingPosition, s"wrong macro implementation: expected one argument, but takes ${args.mkString("(", ", ", ")")}")
    }

    def binaryOperation (operator: (Tree, Tree) => Tree): SemanticActionImpl = SemanticActionImpl { args =>
      if (args.lengthCompare(2) == 0) operator(args.head, args.tail.head)
      else c.abort(c.enclosingPosition, s"wrong macro implementation: expected two argument, but takes ${args.mkString("(", ", ", ")")}")
    }

    def constructorCall (typeName: Tree, parameterCorrespondence: List[Tree] => List[List[Tree]]): SemanticActionImpl = SemanticActionImpl { args =>
      q"new $typeName(...${parameterCorrespondence(args)})"
    }

    def functionCall (functionRef: Tree, parameterCorrespondence: List[Tree] => List[List[Tree]]): SemanticActionImpl = SemanticActionImpl { args =>
      q"$functionRef(...${parameterCorrespondence(args)})"
    }

    def apply (run: List[Tree] => Tree): SemanticActionImpl = new SemanticActionImpl(run)
  }
}
