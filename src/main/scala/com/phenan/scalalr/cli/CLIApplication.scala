package com.phenan.scalalr
package cli

import shared._

import scala.{Console => Stdio}

object CLIApplication extends CLIOptionParserModule with SyntaxFileParserModule
  with CLISyntaxRuleModule with SyntaxRuleModule with LALRAutomatonModule
  with ScalaCodeGeneratorModule with CodeGeneratorModule with ASTDataTypeWriterModule
{
  def applicationMain (args: Array[String]): Unit = optionParser.parse(args, Config()) match {
    case Some(config) if config.syntaxFile != null => run(config)
    case _ => optionParser.showUsage()
  }

  def run (config: Config): Unit = {
    SyntaxParsers.runParser(config.syntaxFile) match {
      case Right(syntax) =>
        if (config.printFlag) printGeneratedCode(syntax.qualifiedName, syntax)
        else writeGeneratedCode(syntax.qualifiedName, syntax, config.directory)
      case Left(msg) =>
        Stdio.err.println(s"invalid syntax file : ${config.syntaxFile}\n  $msg")
    }
  }
}

