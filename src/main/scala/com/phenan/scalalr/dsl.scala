package com.phenan.scalalr

import com.phenan.scalalr.macroimpl._

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

class dsl [T] extends StaticAnnotation {
  def macroTransform (annottees: Any*): Any = macro dslBundle.impl
}

class dslBundle (val c: scala.reflect.macros.whitebox.Context) extends TypingModule with AnnotationFinderModule with MacroModule {
  import c.universe._
  //import Flag._

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
    /*val startRule = getStartRule(declarationBody)
    val derivationRules = collectDerivationRules(declarationBody)
    val branchRules = collectBranchRules(declarationBody)

    val syntax = SyntaxRule(declarationName, startRule, derivationRules ++ branchRules)

    val automaton = LALRAutomaton(syntax)

    println(automaton)*/

    declarationBody.map {
      case ClassDef(mod, name, typeParams, template) => ClassDef(removeSyntaxAnnotation(mod), name, typeParams, template)
      case ModuleDef(mod, name, template) => ModuleDef(removeSyntaxAnnotation(mod), name, template)
      case DefDef(mod, name, typeParams, params, returnType, body) => DefDef(removeSyntaxAnnotation(mod), name, typeParams, params, returnType, body)
      case other => other
    }
  }
/*
  /**
    * 開始記号を発見する関数
    * start アノテーションのついたもの
    * それがなければ syntax アノテーションのついた trait または class のうちはじめに定義されたもの
    * @param declarationBody class, trait, object のボディ
    * @return 開始記号
    */
  private def getStartRule (declarationBody: List[Tree]): NonTerminal = {
    val startTree = declarationBody.find {
      case ClassDef(mod, _, _, _) => mod.annotations.exists(StartAnnotationTree.unapply)
      case _ => false
    } orElse declarationBody.find {
      case ClassDef(mod, _, _, _) => getSyntaxAnnotation(mod).nonEmpty
      case _ => false
    }
    startTree match {
      case Some(ClassDef (_, name, _, _)) => nonTerminal(Ident(name))
      case _ => c.abort(wrappingPos(declarationBody), s"@start can be annotated to class or trait: $startTree")
    }
  }

  private def collectLiteralTokens (declarationBody: List[Tree]): List[LiteralToken] = declarationBody.collect {
    case DefDef(mod, name, Nil, List(List(ValDef(Modifiers(PARAM, _, _), _, typeName, EmptyTree))), returnType, _) if mod.annotations.exists(TokenAnnotationTree.unapply) =>
      ???
  }

  /**
    * 導出規則を集める関数
    * 導出規則は A := x y z のような規則
    * 引数付きの syntax アノテーションのついた case class または case object が導出規則となる
    * @param declarationBody class, trait, object のボディ
    * @return 全ての導出規則
    */
  private def collectDerivationRules (declarationBody: List[Tree]): List[DerivationRule] = declarationBody.flatMap {
    case ClassDef (mod, name, Nil, Template(_, _, classBody)) if mod.hasFlag(CASE) =>
      getSyntaxAnnotation(mod).map { syntax => getDerivationRule(nonTerminal(Ident(name)), classBody, syntax) }
    case ModuleDef (mod, name, Template(_, _, moduleBody)) if mod.hasFlag(CASE) =>
      getSyntaxAnnotation(mod).map { syntax => getDerivationRule(nonTerminal(SingletonTypeTree(Ident(name))), moduleBody, syntax) }
    case _ =>
      None
  }

  /**
    * 分岐規則を集める関数
    * 分岐規則は A := B | C | D のような規則
    * 引数なしの syntax アノテーションのついた sealed class または sealed trait が分岐規則となる
    * @param declarationBody class, trait, object のボディ
    * @return 全ての分岐規則
    */
  private def collectBranchRules (declarationBody: List[Tree]): List[BranchRule] = {
    declarationBody.collect {
      case ClassDef (mod, name, Nil, Template(_, _, _)) if mod.hasFlag(SEALED) =>
        BranchRule(nonTerminal(Ident(name)), declarationBody.collect {
          case ClassDef (_, n, Nil, Template(parents, _, _)) if parents.exists(_.equalsStructure(Ident(name))) => nonTerminal(Ident(n))
          case ModuleDef (_, n, Template(parents, _, _)) if parents.exists(_.equalsStructure(Ident(name))) => nonTerminal(SingletonTypeTree(Ident(n)))
        })
    }
  }

  private def getDerivationRule (left: NonTerminal, classBody: List[Tree], syntax: SyntaxAnnotation): DerivationRule = {
    val caseFields = classBody.collect {
      case ValDef(valMod, valName, valType, EmptyTree) if valMod.hasFlag(CASEACCESSOR | PARAMACCESSOR) =>
        valName -> nonTerminal(valType)
    }.toMap[Name, NonTerminal]
    val operands = syntax.operandTrees.map { case Ident(termName) => caseFields(termName) }
    if (caseFields.size == operands.size)
      DerivationRule(left, (Nil :: operands.map(List(_))).zip(syntax.operators).flatMap(pair => pair._1 ++ pair._2))
    else
      c.abort(syntax.pos, s"@syntax should take ${caseFields.size} operands")
  }

  private def nonTerminal (tree: Tree): NonTerminal = new TypeNonTerminal(tree)

  private class TypeNonTerminal (val tree: Tree) extends NonTerminal {
    override lazy val typeName: String = show(tree)

    override def equals (obj: scala.Any): Boolean = obj match {
      case that: TypeNonTerminal => this.tree.equalsStructure(that.tree)
      case _ => false
    }

    override def toString: String = s"TypeNonTerminal($typeName)"

    override def hashCode (): Int = {
      val prime = 39409
      val result = 83
      result * prime + typeName.hashCode
    }
  }

  private def getSyntaxAnnotation (mod: Modifiers): Option[SyntaxAnnotation] = mod.annotations.collectFirst {
    case SyntaxAnnotationTree(annotation) => annotation
  }
*/

  private case class SyntaxAnnotation (operatorTrees: List[Tree], operandTrees: List[Tree], pos: Position) {
    /*def operators: List[List[Keyword]] = operatorTrees.map {
      case Literal(Constant("")) => Nil
      case Literal(Constant(str: String)) => str.split(" ").filter(_ != "").map(Keyword).toList
      case other => c.abort(other.pos, s"""@syntax only supports g"..." for describing syntax: $other""")
    }
*/
  }

  /*private object SyntaxAnnotationTree {
    def unapply (tree: Tree): Option[SyntaxAnnotation] = tree match {
      case AnnotationTree("syntax", List(Apply(Select(Apply(Ident(TermName("StringContext")), parts), TermName("g")), args))) =>
        Some(SyntaxAnnotation(parts, args, tree.pos))
      case AnnotationTree("syntax", Nil) =>
        Some(SyntaxAnnotation(Nil, Nil, tree.pos))
      case _ =>
        None
    }
  }

  private object StartAnnotationTree {
    def unapply (tree: Tree): Boolean = tree match {
      case AnnotationTree("start", Nil) => true
      case _ => false
    }
  }*/
}
