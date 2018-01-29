package com.phenan.scalalr
package cli

import shared._

import java.io._

import scala.{Console => Stdio}

object CLIApplication extends CLIOptionParserModule with SyntaxFileParserModule
  with CLISyntaxRuleModule with SyntaxRuleModule with LALRAutomatonModule
  with ScalaCodeGeneratorModule with CodeGeneratorModule with ASTDataTypeWriterModule
{
  def applicationMain (args: Array[String]): Unit = optionParser.parse(args, Config()) match {
    case Some(config) if config.syntaxFile != null => run(config)
    case _ => optionParser.showUsage()
  }

  private def run (config: Config): Unit = {
    SyntaxParsers.runParser(config.syntaxFile) match {
      case Right(SyntaxDefinition(qualifiedName, syntax)) =>
        if (config.printFlag) printCode(qualifiedName, syntax)
        else writeCode(qualifiedName, syntax, config.directory)
      case Left(msg) =>
        Stdio.err.println(s"invalid syntax file : ${config.syntaxFile}\n  $msg")
    }
  }

  private def printCode (qualifiedName: List[String], syntax: Syntax): Unit = {
    val writer = new PrintWriter(Stdio.out)
    writeASTDataType(qualifiedName, syntax, writer)
    writeGeneratedDefinitions(qualifiedName, syntax, writer)
  }

  private def writeCode (qualifiedName: List[String], syntax: Syntax, directory: Option[File]): Unit = {
    val dir = directory.getOrElse(new File("."))
    val dslFile = new File(dir, qualifiedName.mkString("/") + ".scala")
    val parent = dslFile.getParentFile
    val astFile = new File(parent, "ASTs.scala")
    parent.mkdirs()

    val writer1 = new PrintWriter(astFile)
    writeASTDataType(qualifiedName, syntax, writer1)
    writer1.close()

    val writer2 = new PrintWriter(dslFile)
    writeGeneratedDefinitions(qualifiedName, syntax, writer2)
    writer2.close()
  }
}
