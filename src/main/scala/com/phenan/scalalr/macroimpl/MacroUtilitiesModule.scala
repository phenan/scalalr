package com.phenan.scalalr.macroimpl

trait MacroUtilitiesModule {
  this: MacroModule =>

  import c.universe._

  object MacroUtilities {
    /**
      * 指定されたモジュールまたはクラス定義にメンバを追加する
      * @param tree モジュールまたはクラス定義
      * @param members メンバ定義
      * @return メンバを追加したモジュールまたはクラス定義
      */
    def addMembers (tree: Tree, members: List[Tree]): Tree = tree match {
      case ModuleDef (mod, name, Template(parents, self, body)) =>
        ModuleDef (mod, name, Template(parents, self, body ++ members))
      case ClassDef (mod, name, typeParams, Template(parents, self, body)) =>
        ClassDef (mod, name, typeParams, Template(parents, self, body ++ members))
      case other =>
        c.abort(other.pos, s"cannot add members to $other")
    }

    /**
      * 全てのコンストラクタ定義を返す関数
      * @param body クラス定義のボディ部分
      * @return 全てのコンストラクタ定義
      */
    def findConstructors (body: List[Tree]): List[DefDef] = body.collect {
      case c @ DefDef(_, termNames.CONSTRUCTOR, _, _, _, _) => c
    }

    /**
      * プライマリコンストラクタを探す関数
      * @param body クラス定義のボディ部分
      * @return Option[プライマリコンストラクタ]
      */
    def findPrimaryConstructor (body: List[Tree]): Option[DefDef] = {
      val paramAccessors = body.collect {
        case ValDef(mods, name, _, _) if mods.hasFlag(Flag.PARAMACCESSOR) => name
      }
      findConstructors(body).find {
        case DefDef(_, termNames.CONSTRUCTOR, _, paramLists, _, _) => paramAccessors.forall { paramName =>
          paramLists.exists {
            _.exists { p => p.mods.hasFlag(Flag.PARAMACCESSOR) && p.name == paramName }
          }
        }
      }
    }
  }
}
