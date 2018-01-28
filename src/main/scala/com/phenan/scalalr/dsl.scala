package com.phenan.scalalr

import shared._
import macroimpl._

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

class dsl [T] extends StaticAnnotation {
  def macroTransform (annottees: Any*): Any = macro dslBundle.impl
}

class dslBundle (val c: scala.reflect.macros.whitebox.Context)
  extends ASTGeneratorModule with CodeGeneratorModule with LALRAutomatonModule
    with SyntaxCollectorModule with TypingModule with AnnotationFinderModule
    with MacroSyntaxRuleModule with SyntaxRuleModule with MacroModule {

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
    * object のボディを変換する
    * @param tree マクロ変換対象となる構文木
    * @return マクロ変換後の構文木
    */
  private def translate (tree: Tree): Tree = tree match {
    case ModuleDef (mod, name, Template(parents, self, body)) =>

      val generated = CodeGenerator(LALRAutomaton(collectSyntax(tree))).generatedDefinitions

      ModuleDef (mod, name, Template(parents, self, removeSyntaxAnnotations(body) ++ generated))
    case other =>
      c.abort(tree.pos, s"@dsl can be annotated to object: $other")
  }

  /**
    * syntax アノテーションは Scala のコンパイルエラーを起こしてしまうため削除する
    * @param declarationBody object定義のボディ
    * @return syntax アノテーションを取り除いたobject定義のボディ
    */
  private def removeSyntaxAnnotations (declarationBody: List[Tree]): List[Tree] = {
    declarationBody.map {
      case ClassDef(mod, name, typeParams, template) => ClassDef(removeSyntaxAnnotation(mod), name, typeParams, template)
      case ModuleDef(mod, name, template) => ModuleDef(removeSyntaxAnnotation(mod), name, template)
      case DefDef(mod, name, typeParams, params, returnType, body) => DefDef(removeSyntaxAnnotation(mod), name, typeParams, params, returnType, body)
      case ValDef(mod, name, valType, body) => ValDef(mod, name, valType, body)
      case other => other
    }
  }
}
