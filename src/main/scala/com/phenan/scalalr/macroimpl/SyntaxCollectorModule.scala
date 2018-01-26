package com.phenan.scalalr
package macroimpl

import shared.SyntaxRuleModule

import java.util.regex.Pattern

trait SyntaxCollectorModule {
  this: SyntaxRuleModule with TypingModule with AnnotationFinderModule with MacroModule =>

  case class NonTerminalImpl (ntType: GenericType)

  override type NonTerminal = NonTerminalImpl
  //override type LiteralToken = this.type

  import c.universe._
  import Flag._


  def collectSyntax (tree: Tree): SyntaxRule = {
    val typeChecker = TypeChecker(tree)

    val start = c.prefix.tree match {
      case Apply(Select(New(AppliedTypeTree(Ident(TypeName("dsl")), List(t))), termNames.CONSTRUCTOR), Nil) => NonTerminalImpl(typeChecker.check(t))
      case _ => c.abort(c.prefix.tree.pos, "@dsl should take a type argument and should not take an argument")
    }

    tree match {
      case ModuleDef (_, name, Template(_, _, body)) =>
        SyntaxRule(packageName.split(Pattern.quote(".")).toList :+ name.toString, start, collectRules(body, typeChecker))
      case other =>
        c.abort(tree.pos, s"@dsl can be annotated to object: $other")
    }
  }

  private def collectRules (body: List[Tree], typeChecker: TypeChecker): List[Rule] = ??? /*collectRules(body, typeChecker, Nil, Nil)

  private def collectRules (body: List[Tree], typeChecker: TypeChecker, derivations: List[DerivationRule], branches: List[(NonTerminal, NonTerminal)]): List[Rule] = {
    body match {
      case ClassDef (mod, name, Nil, Template(parents, _, _)) :: rest if mod.hasFlag(CASE) =>
        val nt = NonTerminalImpl(typeChecker.check(name))
        val (newDerivations, newBranches) = findAnnotation("syntax", mod).foldLeft [(List[DerivationRule], List[(NonTerminal, NonTerminal)])] ((derivations, branches)) {
          case ((ds, bs), List(Apply(Select(Apply(Ident(TermName("StringContext")), parts), TermName("g")), args))) =>
            ( ???, bs ++ parents.map(p => nt -> NonTerminalImpl(typeChecker.check(p))) :+ ??? )
          case ((ds, bs), Nil) =>
            ( ds, bs ++ parents.map(p => nt -> NonTerminalImpl(typeChecker.check(p))) )
          case ((ds, bs), other) =>
            c.abort(body.head.pos, s"""@syntax only supports g"..." for describing syntax: $other""")
        }
        collectRules(rest, typeChecker, newDerivations, newBranches)
      case Nil =>
        ???
    }
  }*/
}
