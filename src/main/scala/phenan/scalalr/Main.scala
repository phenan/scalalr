package phenan.scalalr

import java.io._

/**
  * Created by @phenan on 2016/12/19.
  */
object Main {
  def main (args: Array[String]): Unit = args.foreach { file =>
    SyntaxParsers.parseAll(SyntaxParsers.syntax, new BufferedReader(new FileReader(file))).map { syntax =>
      println(CodeGenerator(LALRAutomaton(syntax)).parserProgram)
    }
  }
}
