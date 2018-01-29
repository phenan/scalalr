package com.phenan.scalalr
package cli

import shared._

import java.io._

import shapeless._

trait ASTDataTypeWriterModule {
  this: CodeGeneratorModule with CLISyntaxRuleModule with SyntaxRuleModule =>

  import output._

  def writeASTDataType (qualifiedName: List[String], syntax: Syntax, writer: PrintWriter): Unit = {
    if (qualifiedName.init.nonEmpty) {
      writer.println(s"package ${qualifiedName.init.mkString(".")}")
    }
    writer.println(generateProgram(astDataTypeDefinitions(syntax)))
    writer.flush()
  }

  private def astDataTypeDefinitions (syntax: Syntax): List[MemberDef] = syntax.nonTerminals.toList.flatMap { nt =>
    syntax.rules.find(_.left == nt).map {
      case Rule(_, _, Branch)         => sealedTraitDef(nt.name, findSuperType(nt, syntax).map(nonTerminalType))
      case Rule(_, right, Derivation) => caseClassDef(nt.name, Nil, collectDerivationDataParameters(right), findSuperType(nt, syntax).map(nonTerminalType))
    }
  }

  private def findSuperType (nt: NonTerminal, syntax: Syntax): Option[NonTerminal] = syntax.rules.collectFirst {
    case Rule(left, right, Branch) if right == List(Symbol(nt)) => left
  }

  private def collectDerivationDataParameters (right: List[Symbol]): List[Parameter] = right.collect {
    case Inl(nt)            => nonTerminalType(nt)
    case Inr(Inl(Inl(lit))) => literalType(lit)
  }.zipWithIndex.map {
    case (t, n) => parameter(s"arg$n", t)
  }
}
