package com.phenan.scalalr

import java.io._

import scala.{Console => Stdio}
import scopt._

object CommandLineApplication extends CommandLineApplicationModule with SyntaxRuleModule with LALRAutomatonModule with CodeGeneratorModule {
  def main (args: Array[String]): Unit = optionParser.parse(args, Config()) match {
    case Some(config) if config.syntaxFile != null => run(config)
    case _ => optionParser.showUsage()
  }

  def run (config: Config): Unit = {
    val reader = new BufferedReader(new FileReader(config.syntaxFile))
    val parseResult = SyntaxParsers.parseAll(SyntaxParsers.syntax, reader)
    reader.close()

    parseResult match {
      case SyntaxParsers.Success(syntax, _) =>
        val gen = CodeGenerator(LALRAutomaton(syntax))
        val out = config.dslOutputFile.getOrElse(new File(syntax.name + ".scala"))

        if (config.printFlag) {
          println("/***********************/")
          println(gen.generatedCodeOfASTDataTypeDefinitions)
          println("\n/***********************/\n")
          println(gen.generatedCodeWithoutDataDefinitions)
        }
        else config.astOutputFile match {
          case Some(a) =>
            val writer1 = new BufferedWriter(new FileWriter(a))
            writer1.write(gen.generatedCodeOfASTDataTypeDefinitions)
            writer1.close()

            val writer2 = new BufferedWriter(new FileWriter(out))
            writer2.write(gen.generatedCodeWithoutDataDefinitions)
            writer2.close()
          case None    =>
            val writer = new BufferedWriter(new FileWriter(out))
            writer.write(gen.generatedCodeWithDataDefinitions)
            writer.close()
        }

      case SyntaxParsers.NoSuccess(msg, _)  =>
        Stdio.err.println(s"invalid syntax file : ${config.syntaxFile}\n  $msg")
    }
  }

  case class Config (astOutputFile: Option[File] = None, dslOutputFile: Option[File] = None, printFlag: Boolean = false, syntaxFile: File = null)

  private val optionParser: OptionParser[Config] = new OptionParser[Config] ("scalalr") {
    head("ScaLALR", "1.1")

    opt[File] ('a', "ast-decl").action((f, c) => c.copy(astOutputFile = Some(f)))
      .text("target file name to write the AST declarations")

    opt[File] ('d', "dsl-decl").action((f, c) => c.copy(dslOutputFile = Some(f)))
      .text("target file name to write the generated code out")

    opt[Unit] ('p', "print").action((_, c) => c.copy(printFlag = true))
      .text("print the generated code")

    help("help")
      .text("print this usage text")

    arg[File]("<file>").action((f, c) => c.copy(syntaxFile = f))
      .text("input syntax file")
  }
}
