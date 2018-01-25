package com.phenan.scalalr

import cli._
import shared._

import java.io._

import scala.{Console => Stdio}
import scopt._

object CommandLineApplication extends CommandLineApplicationModule with SyntaxRuleModule with LALRAutomatonModule with CodeGeneratorModule {
  def main (args: Array[String]): Unit = optionParser.parse(args, Config()) match {
    case Some(config) if config.syntaxFile != null => run(config)
    case _ => optionParser.showUsage()
  }

  def run (config: Config): Unit = {
    SyntaxParsers.runParser(config.syntaxFile) match {
      case Right(syntax) =>
        if (config.printFlag) printGeneratedCode(syntax)
        else writeGeneratedCode(syntax, config.directory)
      case Left(msg) =>
        Stdio.err.println(s"invalid syntax file : ${config.syntaxFile}\n  $msg")
    }
  }

  def printGeneratedCode (syntax: SyntaxRule): Unit = {
    val gen = CodeGenerator(LALRAutomaton(syntax))
    println("/***********************/")
    println(gen.generateCode(gen.astDataTypeDefinitions))
    println("\n/***********************/\n")
    println(gen.generateCode(gen.program))
  }

  def writeGeneratedCode (syntax: SyntaxRule, directory: Option[File]): Unit = {
    val dir = directory.getOrElse(new File("."))
    val dslFile = new File(dir, syntax.qualifiedName.mkString("/") + ".scala")
    val parent = dslFile.getParentFile
    val astFile = new File(parent, "ASTs.scala")
    parent.mkdirs()

    val gen = CodeGenerator(LALRAutomaton(syntax))
    val writer1 = new BufferedWriter(new FileWriter(astFile))
    if (syntax.qualifiedName.init.nonEmpty) {
      writer1.write(s"package ${syntax.qualifiedName.init.mkString(".")}")
      writer1.newLine()
    }
    writer1.write(gen.generateCode(gen.astDataTypeDefinitions))
    writer1.close()

    val writer2 = new BufferedWriter(new FileWriter(dslFile))
    if (syntax.qualifiedName.init.nonEmpty) {
      writer2.write(s"package ${syntax.qualifiedName.init.mkString(".")}")
      writer2.newLine()
      writer2.newLine()
      writer2.write("import com.phenan.scalalr._")
      writer2.newLine()
    }
    writer2.write(gen.generateCode(gen.program))
    writer2.close()
  }

  case class Config
  (directory         : Option[File] = None,
   printFlag         : Boolean = false,
   syntaxFile        : File = null)

  private val optionParser: OptionParser[Config] = new OptionParser[Config] ("scalalr") {
    head("ScaLALR", "1.2")

    opt[File] ('d', "directory").action((f, c) => c.copy(directory = Some(f)))
      .text("target file name to write the generated code out")

    opt[Unit] ('p', "print").action((_, c) => c.copy(printFlag = true))
      .text("print the generated code")

    help("help")
      .text("print this usage text")

    arg[File]("<file>").action((f, c) => c.copy(syntaxFile = f))
      .text("input syntax file")
  }
}
