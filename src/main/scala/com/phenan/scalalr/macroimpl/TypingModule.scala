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

  case class ScalaClass (symbol: Symbol) extends GenericClass

  sealed trait GenericObject

  case class DSLObject (outerName: TermName, moduleName: TermName) extends GenericObject

  case class ScalaObject (symbol: Symbol) extends GenericObject

  lazy val packageName: String = {
    c.typecheck(q"object ${TermName(c.freshName("ScaLALR$"))}").symbol.owner.fullName
  }

  class TypeChecker (outerName: TermName, classes: Map[TypeName, List[Tree]], modules: Map[TermName, List[Tree]]) {
    def check (tree: Tree): GenericType = treeToTypeMemo.getOrElseUpdate(tree, resolveType(tree))

    def superTypes (t: GenericType): Set[GenericType] = superTypesMemo.getOrElseUpdate(t, getSuperTypes(t))

    private def resolveType (tree: Tree): GenericType = tree match {
      case AppliedTypeTree(typeConstructor, args) =>
        ParameterizedType(resolveTypeConstructor(typeConstructor, args.length), args.map(check))
      case SingletonTypeTree(objectRef) =>
        SingletonType(resolveSingleton(objectRef))
      case other =>
        SimpleType(resolveTypeConstructor(other, 0))
    }

    private def resolveTypeConstructor (tree: Tree, argNum: Int): GenericClass = tree match {
      case Ident(name : TypeName) if classes.contains(name)                                                           =>
        DSLClass(outerName, name)
      case Select(Ident(outer), (name : TypeName)) if outer == outerName && classes.contains(name)                    =>
        DSLClass(outerName, name)
      case Select(outer, (name : TypeName)) if show(outer) == packageName + "." + outerName && classes.contains(name) =>
        DSLClass(outerName, name)
      case _ if argNum > 0                                                                                            =>
        ScalaClass(c.typecheck(q"??? : $tree[..${(0 until argNum).map(_ => "_")}]").tpe.dealias.typeConstructor.typeSymbol)
      case _                                                                                                          =>
        ScalaClass(c.typecheck(q"??? : $tree").tpe.dealias.typeConstructor.typeSymbol)
    }

    private def resolveSingleton (tree: Tree): GenericObject = tree match {
      case Ident(name: TermName) if modules.contains(name)                                                            =>
        DSLObject(outerName, name)
      case Select(Ident(outer), (name : TermName)) if outer == outerName && modules.contains(name)                    =>
        DSLObject(outerName, name)
      case Select(outer, (name : TermName)) if show(outer) == packageName + "." + outerName && modules.contains(name) =>
        DSLObject(outerName, name)
      case _                                                                                                          =>
        ScalaObject(c.typecheck(q"??? : $tree").tpe.dealias.termSymbol)
    }

    private def getSuperTypes (t: GenericType): Set[GenericType] = t match {
      case ParameterizedType(_, _) => Set.empty                // currently not supported
      case SimpleType(clazz)       => getSuperTypes(clazz)
      case SingletonType(obj)      => getSuperTypes(obj)
    }

    private def getSuperTypes (clazz: GenericClass): Set[GenericType] = clazz match {
      case DSLClass(_, name) =>
        val directSuperClasses = classes(name).map(check).toSet
        directSuperClasses ++ directSuperClasses.flatMap(superTypes)
      case ScalaClass(symbol) =>
        symbol.typeSignature.baseClasses.filterNot(_ == symbol).map { s =>
          typeToGenericType(symbol.typeSignature.baseType(s))
        }.toSet
    }

    private def getSuperTypes (obj: GenericObject): Set[GenericType] = obj match {
      case DSLObject(_, name) =>
        val directSuperClasses = modules(name).map(check).toSet
        directSuperClasses ++ directSuperClasses.flatMap(superTypes)
      case ScalaObject(symbol) =>
        symbol.typeSignature.baseClasses.filterNot(_ == symbol.typeSignature.typeSymbol).map { s =>
          typeToGenericType(symbol.typeSignature.baseType(s))
        }.toSet
    }

    private def typeToGenericType (t: Type): GenericType = {
      val clazz = ScalaClass(t.dealias.typeConstructor.typeSymbol)
      if (t.typeArgs.isEmpty) SimpleType(clazz)
      else ParameterizedType(clazz, t.typeArgs.map(typeToGenericType))
    }

    private val treeToTypeMemo: scala.collection.mutable.Map[Tree, GenericType] = scala.collection.mutable.Map.empty
    private val superTypesMemo: scala.collection.mutable.Map[GenericType, Set[GenericType]] = scala.collection.mutable.Map.empty
  }

  object TypeChecker {
    def apply (tree: Tree): TypeChecker = tree match {
      case ModuleDef (_, name, Template(_, _, body)) =>
        TypeChecker(name, body)
      case other =>
        c.abort(tree.pos, s"@dsl can be annotated to object: $other")
    }

    def apply (outerName: TermName, body: List[Tree]): TypeChecker = {
      val classNames = body.collect { case ClassDef(_, name, _, Template(parents, _, _)) => (name, parents) }.toMap
      val moduleNames = body.collect { case ModuleDef(_, name, Template(parents, _, _)) => (name, parents) }.toMap
      new TypeChecker(outerName, classNames, moduleNames)
    }
  }
}
