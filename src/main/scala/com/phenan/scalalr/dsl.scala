package com.phenan.scalalr

import shared._
import macroimpl._

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

class dsl [T] extends StaticAnnotation {
  def macroTransform (annottees: Any*): Any = macro dslBundle.impl
}

class dslBundle (val c: scala.reflect.macros.whitebox.Context)
  extends TreeGeneratorModule with CodeGeneratorModule with LALRAutomatonModule
    with SyntaxGeneratorModule with TyperModule with SyntaxInfoCollectorModule with SyntaxInfoModule
    with SyntaxRuleModule with AnnotationFinderModule with MacroModule
{
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
  private def translate (tree: Tree): Tree = {
    val (t, s) = processSyntaxAnnotations(tree)
    doTypeCheck(t)
    val syntax = generateSyntax(s)
    val generated = CodeGenerator(LALRAutomaton(syntax)).generatedDefinitions
    addMembers(t, generated)
  }

  private def addMembers (tree: Tree, members: List[Tree]): Tree = tree match {
    case ModuleDef (mod, name, Template(parents, self, body)) =>
      ModuleDef (mod, name, Template(parents, self, body ++ members))
    case ClassDef (mod, name, typeParams, Template(parents, self, body)) =>
      ClassDef (mod, name, typeParams, Template(parents, self, body ++ members))
    case other =>
      c.abort(other.pos, s"cannot add members to $other")
  }
}

/*
class dslBundle (val c: scala.reflect.macros.whitebox.Context)
  extends ASTGeneratorModule with CodeGeneratorModule with LALRAutomatonModule
    with SyntaxCollectorModule with TypingModule with AnnotationFinderModule with TyperModule
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

      val typer = Typer(ModuleDef(mod, name, Template(parents, self, removeSyntaxAnnotations(body))))

      val e1 = typer.check(tq"List[String]")
      val e2 = typer.check(tq"scala.collection.immutable.List[java.lang.String]")
      println("e1: " + e1)
      println("e2: " + e2)

      println("e1 hash : " + e1.hashCode())
      println("e2 hash : " + e2.hashCode())
      println("e1 == e2 : " + (e1 == e2))

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
*/
