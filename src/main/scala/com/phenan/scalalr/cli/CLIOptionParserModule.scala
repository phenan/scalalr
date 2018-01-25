package com.phenan.scalalr.cli

import java.io.File

import scopt._

trait CLIOptionParserModule {
  case class Config
  (directory         : Option[File] = None,
   printFlag         : Boolean = false,
   syntaxFile        : File = null)

  val optionParser: OptionParser[Config] = new OptionParser[Config] ("scalalr") {
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
