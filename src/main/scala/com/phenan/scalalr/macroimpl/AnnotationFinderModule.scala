package com.phenan.scalalr.macroimpl

trait AnnotationFinderModule {
  this: MacroModule =>

  import c.universe._

  def findAnnotation (name: String, modifiers: Modifiers): List[List[Tree]] = modifiers.annotations.collect {
    case AnnotationTree(n, args) if n == name => args
  }

  def removeSyntaxAnnotation (modifiers: Modifiers): Modifiers = {
    Modifiers(modifiers.flags, modifiers.privateWithin, modifiers.annotations.filterNot {
      case AnnotationTree("syntax", _) => true
      case _ => false
    })
  }

  object AnnotationTree {
    def unapply (tree: Tree): Option[(String, List[Tree])] = tree match {
      case Apply(Select(New(Ident(TypeName(annName))), termNames.CONSTRUCTOR), args) => Some((annName, args))
      case _ => None
    }
  }

  object TypeWithSyntaxAnnotation {
    def unapply (tree: Tree): Option[(List[List[Tree]], Tree)] = unapplyHelper(tree, Nil)

    private def unapplyHelper (tree: Tree, anns: List[List[Tree]]): Option[(List[List[Tree]], Tree)] = tree match {
      case Annotated(AnnotationTree("syntax", args), t) => unapplyHelper(t, anns :+ args)
      case t => Some(anns -> t)
    }
  }

  object TypeWithSepAnnotation {
    def unapply (tree: Tree): Option[(Option[String], Tree)] = tree match {
      case Annotated(AnnotationTree("sep", List(Literal(Constant(sep: String)))), t) => Some((Some(sep), t))
      case t => Some((None, t))
    }
  }
}
