package com.phenan.scalalr.macroimpl

trait TypingModule {
  this: MacroModule =>

  import c.universe._

  sealed trait GenericType

  case class SimpleType (clazz: GenericClass) extends GenericType

  case class ParameterizedType (clazz: GenericClass, args: List[GenericType]) extends GenericType

  case class SingletonType (objectRef: GenericObject) extends GenericType

  sealed trait GenericClass

  case class DSLClass (outerName: TermName, className: TypeName) extends GenericClass

  case class ScalaClass (typeConstructor: Type) extends GenericClass

  sealed trait GenericObject

  case class DSLObject (outerName: TermName, moduleName: TermName) extends GenericObject

  case class ScalaObject (objectType: Type) extends GenericObject

  lazy val packageName: String = {
    c.typecheck(q"object ${TermName(c.freshName("ScaLALR$"))}").symbol.owner.fullName
  }

  class TypeChecker (outerName: TermName, classNames: List[TypeName], moduleNames: List[TermName]) {
    def check (tree: Tree): GenericType = tree match {
      case AppliedTypeTree(typeConstructor, args) =>
        ParameterizedType(resolveTypeConstructor(typeConstructor, args.length), args.map(check))
      case SingletonTypeTree(objectRef) =>
        SingletonType(resolveSingleton(objectRef))
      case other =>
        SimpleType(resolveTypeConstructor(other, 0))
    }

    def check (name: TypeName): GenericType = SimpleType(resolveTypeConstructor(Ident(name), 0))

    private def resolveTypeConstructor (tree: Tree, argNum: Int): GenericClass = tree match {
      case Ident(name : TypeName) if classNames.contains(name) =>
        DSLClass(outerName, name)
      case Select(Ident(outer), (name : TypeName)) if outer == outerName && classNames.contains(name) =>
        DSLClass(outerName, name)
      case Select(outer, (name : TypeName)) if show(outer) == packageName + "." + outerName && classNames.contains(name) =>
        DSLClass(outerName, name)
      case _ if argNum > 0 =>
        ScalaClass(c.typecheck(q"??? : $tree[..${(0 until argNum).map(_ => "_")}]").tpe.dealias.typeConstructor)
      case _ =>
        ScalaClass(c.typecheck(q"??? : $tree").tpe.dealias.typeConstructor)
    }

    private def resolveSingleton (tree: Tree): GenericObject = tree match {
      case Ident(name: TermName) if moduleNames.contains(name) =>
        DSLObject(outerName, name)
      case Select(Ident(outer), (name : TermName)) if outer == outerName && moduleNames.contains(name) =>
        DSLObject(outerName, name)
      case Select(outer, (name : TermName)) if show(outer) == packageName + "." + outerName && moduleNames.contains(name) =>
        DSLObject(outerName, name)
      case _ =>
        ScalaObject(c.typecheck(q"??? : $tree").tpe.dealias)
    }
  }

  object TypeChecker {
    def apply (tree: Tree): TypeChecker = tree match {
      case ModuleDef (_, name, Template(_, _, body)) =>
        TypeChecker(name, body)
      case other =>
        c.abort(tree.pos, s"@dsl can be annotated to object: $other")
    }

    def apply (outerName: TermName, body: List[Tree]): TypeChecker = {
      val classNames = body.collect { case ClassDef(_, name, _, _) => name }
      val moduleNames = body.collect { case ModuleDef(_, name, _) => name }
      new TypeChecker(outerName, classNames, moduleNames)
    }
  }
}
