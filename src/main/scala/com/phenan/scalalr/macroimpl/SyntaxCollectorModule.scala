package com.phenan.scalalr.macroimpl

import com.phenan.scalalr.SyntaxRuleModule

trait SyntaxCollectorModule {
  this: SyntaxRuleModule with TypingModule with AnnotationFinderModule with MacroModule =>

  case class NonTerminalImpl (ntType: GenericType)

  override type NonTerminal = NonTerminalImpl
  //override type LiteralToken = this.type

  import c.universe._



  def collectSyntax (tree: Tree): SyntaxRule = {
    val typeChecker = TypeChecker(tree)

    val start = c.prefix.tree match {
      case Apply(Select(New(AppliedTypeTree(Ident(TypeName("dsl")), List(t))), termNames.CONSTRUCTOR), Nil) => NonTerminalImpl(typeChecker.check(t))
      case _ => c.abort(c.prefix.tree.pos, "@dsl should take a type argument and should not take an argument")
    }

    SyntaxRule(???, start, ???)
  }
}
