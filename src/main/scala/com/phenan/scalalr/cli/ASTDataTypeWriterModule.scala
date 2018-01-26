package com.phenan.scalalr
package cli

import shared._

import java.io._

import shapeless._

trait ASTDataTypeWriterModule {
  this: CodeGeneratorModule with CLISyntaxRuleModule =>

  import output._

  def writeASTDataType (syntax: SyntaxRule, writer: PrintWriter): Unit = {
    if (syntax.qualifiedName.init.nonEmpty) {
      writer.println(s"package ${syntax.qualifiedName.init.mkString(".")}")
    }
    writer.println(generateProgram(astDataTypeDefinitions(syntax)))
    writer.flush()
  }

  private def astDataTypeDefinitions (syntax: SyntaxRule): List[MemberDef] = syntax.nonTerminals.toList.flatMap { nt =>
    syntax.rules.find(_.left == nt).map {
      case BranchRule(_, _)         => sealedTraitDef(nt.name, findSuperType(nt, syntax).map(nonTerminalType))
      case DerivationRule(_, right) => caseClassDef(nt.name, Nil, collectDerivationDataParameters(right), findSuperType(nt, syntax).map(nonTerminalType))
    }
  }

  private def findSuperType (nt: NonTerminal, syntax: SyntaxRule): Option[NonTerminal] = syntax.rules.collectFirst {
    case BranchRule(left, right) if right.contains(nt) => left
  }

  private def collectDerivationDataParameters (right: List[Symbol]): List[Parameter] = right.collect {
    case Inl(nt)            => nonTerminalType(nt)
    case Inr(Inl(Inl(lit))) => literalType(lit)
  }.zipWithIndex.map {
    case (t, n) => parameter(s"arg$n", t)
  }
}
