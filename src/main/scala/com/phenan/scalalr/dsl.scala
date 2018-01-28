package com.phenan.scalalr

import shared._
import macroimpl._

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

class dsl [T] extends StaticAnnotation {
  def macroTransform (annottees: Any*): Any = macro dslBundle.impl
}

class dslBundle (val c: scala.reflect.macros.whitebox.Context) extends SyntaxCollectorModule with TypingModule with AnnotationFinderModule with MacroSyntaxRuleModule with SyntaxRuleModule with MacroModule {
  import c.universe._

  /**
    * マクロのエントリーポイント
    * @param annottees マクロ変換対象となる構文木
    * @return マクロ変換後の構文木
    */
  def impl (annottees: Tree*): Tree = {
    q"""
      ${translate(annottees.head)}
      ..${annottees.tail}
      """
  }

  /**
    * class, trait, object のボディを変換する
    * @param tree マクロ変換対象となる構文木
    * @return マクロ変換後の構文木
    */
  private def translate (tree: Tree): Tree = tree match {
    case ClassDef (mod, name, typeParams, Template(parents, self, body)) =>
      ClassDef (mod, name, typeParams, Template(parents, self, translateDeclarationBody(name.toString, body)))
    case ModuleDef (mod, name, Template(parents, self, body)) =>
      println(collectSyntax(tree))
      ModuleDef (mod, name, Template(parents, self, translateDeclarationBody(name.toString, body)))
    case other =>
      c.abort(tree.pos, s"@dsl can be annotated to class, trait, or object: $other")
  }

  /**
    * マクロ変換本体
    * @param declarationName class, trait, object の名前
    * @param declarationBody class, trait, object のボディ
    * @return マクロ変換後のボディ
    */
  private def translateDeclarationBody (declarationName: String, declarationBody: List[Tree]): List[Tree] = {
    declarationBody.map {
      case ClassDef(mod, name, typeParams, template) => ClassDef(removeSyntaxAnnotation(mod), name, typeParams, template)
      case ModuleDef(mod, name, template) => ModuleDef(removeSyntaxAnnotation(mod), name, template)
      case DefDef(mod, name, typeParams, params, returnType, body) => DefDef(removeSyntaxAnnotation(mod), name, typeParams, params, returnType, body)
      case ValDef(mod, name, valType, body) => ValDef(mod, name, valType, body)
      case other => other
    }
  }
}
