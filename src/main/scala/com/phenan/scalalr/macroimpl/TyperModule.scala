package com.phenan.scalalr.macroimpl

import java.util.regex.Pattern

trait TyperModule {
  this: MacroModule =>

  import c.universe._

  def typer: Typer = typerVar

  def doTypeCheck (tree: Tree): Unit = {
    typerVar = Typer(tree)
  }

  private var typerVar: Typer = _

  case class Typer private (classTypes: Map[TypeName, Type], moduleTypes: Map[TermName, Type], outerName: TermName) {

    lazy val packageName: String = c.typecheck(q"object ${TermName(c.freshName("ScaLALR$"))}").symbol.owner.fullName

    def check (tree: Tree): Type = treeToTypeMemo.getOrElseUpdate(tree, resolveType(tree))

    def unchecked (t: Type): Tree = {
      if (t.typeArgs.nonEmpty) {
        tq"${stringToTypeTree(t.typeConstructor.typeSymbol.fullName)}[..${t.typeArgs.map(unchecked)}]"
      }
      else {
        classTypes_rev.get(t).map(clazz => tq"$clazz")
          .orElse(moduleTypes_rev.get(t).map(module => tq"$module.type"))
          .getOrElse(tq"$t")
      }
    }

    def stringToTypeTree (string: String): Tree = {
      val dot = string.lastIndexOf('.')
      val prefix = string.take(dot)
      val postfix = string.drop(dot + 1)

      if (prefix.isEmpty) tq"${TypeName(postfix)}"
      else tq"${stringToQualifiedTerm(prefix)}.${TypeName(postfix)}"
    }

    def stringToQualifiedTerm (string: String): Tree = {
      val terms = string.split(Pattern.quote(".")).map(TermName(_))
      terms.tail.foldLeft [Tree] (q"${terms.head}") { (left, term) => q"$left.$term" }
    }

    private lazy val classTypes_rev = classTypes.map { case (k, v) => (v, k) }
    private lazy val moduleTypes_rev = moduleTypes.map { case (k, v) => (v, k) }

    private def resolveType (tree: Tree): Type = tree match {
      case Ident(name: TypeName) if classTypes.contains(name) =>
        classTypes(name)
      case Select(qualifier, name: TypeName) if qualifierIsOuter(qualifier) && classTypes.contains(name) =>
        classTypes(name)
      case SingletonTypeTree(Ident(name: TermName)) if moduleTypes.contains(name) =>
        moduleTypes(name)
      case SingletonTypeTree(Select(qualifier, name: TermName)) if qualifierIsOuter(qualifier) && moduleTypes.contains(name) =>
        moduleTypes(name)
      case Ident(_) =>
        c.typecheck(tree, c.TYPEmode).tpe.dealias
      case Select(_, _) =>
        c.typecheck(tree, c.TYPEmode).tpe.dealias
      case SingletonTypeTree(_) =>
        c.typecheck(tree, c.TYPEmode).tpe.dealias
      case AppliedTypeTree(typeConstructor, args) =>
        c.typecheck(tq"$typeConstructor[..${args.map(check)}]", c.TYPEmode).tpe.dealias
      case _ =>
        c.typecheck(tree, c.TYPEmode).tpe.dealias
    }

    private def qualifierIsOuter (qualifier: Tree): Boolean = qualifier match {
      case Ident(name) => name == outerName
      case Select(pack, name) => show(pack) == packageName && name == outerName
    }

    private val treeToTypeMemo: scala.collection.mutable.Map[Tree, Type] = scala.collection.mutable.Map.empty
  }

  object Typer {
    def apply (tree: Tree): Typer = tree match {
      case m : ModuleDef =>
        moduleDefTypeChecker(m)
      case other =>
        c.abort(tree.pos, s"@dsl can be annotated to object: $other")
    }

    private def moduleDefTypeChecker (m: ModuleDef): Typer = {
      val ModuleDef(mod, name, Template(parents, self, body)) = m

      val classDefs = collectClassDefs(body)
      val moduleDefs = collectModuleDefs(body)

      val nameAndType_Class = classDefs.map(TermName(c.freshName("ScaLALR$")) -> _.name).toMap
      val nameAndType_Module = moduleDefs.map(TermName(c.freshName("ScaLALR$")) -> _.name).toMap

      val classExprs = nameAndType_Class.map { case (n, t) => q"def $n : $t = null" }.toList
      val moduleExprs = nameAndType_Module.map { case (n, t) => q"def $n : $t.type = null" }.toList

      val moduleDef = ModuleDef(mod, name, Template(parents, self, classDefs ++ moduleDefs ++ classExprs ++ moduleExprs))

      val ModuleDef(_, _, Template(_, _, trees)) = c.typecheck(moduleDef)

      val classTypes = trees.collect {
        case DefDef(_, n, _, _, t, _) if nameAndType_Class.contains(n) => nameAndType_Class(n) -> t.tpe.dealias
      }.toMap

      val moduleTypes = trees.collect {
        case DefDef(_, n, _, _, t, _) if nameAndType_Module.contains(n) => nameAndType_Module(n) -> t.tpe.dealias
      }.toMap

      Typer(classTypes, moduleTypes, name)
    }

    private def collectClassDefs (body: List[Tree]) = body.collect { case c: ClassDef => c }
    private def collectModuleDefs (body: List[Tree]) = body.collect { case m: ModuleDef => m }
  }
}
