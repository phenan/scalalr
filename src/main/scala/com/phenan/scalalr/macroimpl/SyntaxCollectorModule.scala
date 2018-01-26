package com.phenan.scalalr
package macroimpl

import shared._

import java.util.regex.Pattern

trait SyntaxCollectorModule {
  this: MacroSyntaxRuleModule with SyntaxRuleModule with TypingModule with AnnotationFinderModule with MacroModule =>

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

  private def collectRules (body: List[Tree], typeChecker: TypeChecker): List[Rule] = collectRules(body, typeChecker, Nil)

  private def collectRules (body: List[Tree], typeChecker: TypeChecker, rules: List[Rule]): List[Rule] = {
    body match {
      case ClassDef (mod, name, Nil, Template(parents, _, _)) :: rest if mod.hasFlag(CASE) =>
        val nt = NonTerminalImpl(typeChecker.check(name))
        val collectedRules = findAnnotation("syntax", mod).foldLeft [List[Rule]] (rules) {
          case (rs, List(Apply(Select(Apply(Ident(TermName("StringContext")), parts), TermName("g")), args))) =>
            ???
            //( ???, bs ++ parents.map(p => nt -> NonTerminalImpl(typeChecker.check(p))) :+ ??? )
          case (rs, Nil) =>
            ???
          //( ds, bs ++ parents.map(p => nt -> NonTerminalImpl(typeChecker.check(p))) )
          case (rs, other) =>
            c.abort(body.head.pos, s"""@syntax only supports g"..." for describing syntax: $other""")
        }
        collectRules(rest, typeChecker, collectedRules)
      case Nil =>
        ???
    }
  }
}
