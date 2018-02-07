package com.phenan.scalalr.macroimpl

trait AnnotationFinderModule {
  this: MacroModule =>

  import c.universe._

  def findSyntaxAnnotation (modifiers: Modifiers): List[List[Tree]] = modifiers.annotations.collect {
    case AnnotationTree(ann, args) if c.typecheck(ann, c.TYPEmode).tpe =:= syntaxType => args
  }

  def removeSyntaxAnnotation (modifiers: Modifiers): Modifiers = {
    Modifiers(modifiers.flags, modifiers.privateWithin, modifiers.annotations.filterNot {
      case AnnotationTree(ann, _) if c.typecheck(ann, c.TYPEmode).tpe =:= syntaxType => true
      case _ => false
    })
  }

  object AnnotationTree {
    /**
      * アノテーションを表現する構文木を分解して、アノテーションの型を表す構文木と引数部分の構文木に分ける Extractor
      * @param tree アノテーションを表現する構文木
      * @return Option[(アノテーションの型を表す構文木, 引数部分の構文木)]
      */
    def unapply (tree: Tree): Option[(Tree, List[Tree])] = tree match {
      case Apply(Select(New(annType), termNames.CONSTRUCTOR), args) => Some((annType, args))
      case _ => None
    }
  }

  object TypeWithSyntaxAnnotation {
    def unapply (tree: Tree): Option[(List[List[Tree]], Tree)] = unapplyHelper(tree, Nil)

    private def unapplyHelper (tree: Tree, anns: List[List[Tree]]): Option[(List[List[Tree]], Tree)] = tree match {
      case Annotated(AnnotationTree(ann, args), t) if c.typecheck(ann, c.TYPEmode).tpe =:= syntaxType => unapplyHelper(t, anns :+ args)
      case t => Some(anns -> t)
    }
  }

  object TypeWithSepAnnotation {
    def unapply (tree: Tree): Option[(Option[String], Tree)] = tree match {
      case Annotated(AnnotationTree(ann, List(Literal(Constant(sep: String)))), t) if c.typecheck(ann, c.TYPEmode).tpe =:= sepType => Some((Some(sep), t))
      case t => Some((None, t))
    }
  }

  private lazy val syntaxType = c.typecheck(tq"com.phenan.scalalr.syntax", c.TYPEmode).tpe
  private lazy val sepType = c.typecheck(tq"com.phenan.scalalr.sep", c.TYPEmode).tpe
}
