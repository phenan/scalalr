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
}
