package com.phenan.scalalr.macroimpl

trait SyntaxInfoCollectorModule {
  this: AnnotationFinderModule with SyntaxInfoModule with MacroModule =>

  import c.universe._

  /**
    * syntax アノテーションを処理して、文法規則の情報を集め syntax アノテーションを削除した構文木を返す関数
    * @param tree 対象の構文木
    * @return (syntax アノテーションを除去した構文木, 文法規則の情報のリスト)
    */
  def processSyntaxAnnotations (tree: Tree): (Tree, List[SyntaxInfo]) = tree match {
    case ClassDef(mod, name, typeParams, Template(parents, self, body)) =>
      (ClassDef(mod, name, typeParams, Template(parents, self, removeSyntaxAnnotations(body))), collectRules(body))
    case ModuleDef(mod, name, Template(parents, self, body)) =>
      (ModuleDef(mod, name, Template(parents, self, removeSyntaxAnnotations(body))), collectRules(body))
  }

  /**
    * syntax アノテーションは Scala のコンパイルエラーを起こしてしまうため削除する
    * @param trees 変換対象の構文木のリスト
    * @return syntax アノテーションを取り除いた構文木のリスト
    */
  private def removeSyntaxAnnotations (trees: List[Tree]): List[Tree] = trees.map {
    case ClassDef(mod, name, typeParams, template) => ClassDef(removeSyntaxAnnotation(mod), name, typeParams, template)
    case ModuleDef(mod, name, template) => ModuleDef(removeSyntaxAnnotation(mod), name, template)
    case DefDef(mod, name, typeParams, params, returnType, body) => DefDef(removeSyntaxAnnotation(mod), name, typeParams, params, returnType, body)
    case ValDef(mod, name, valType, body) => ValDef(mod, name, valType, body)
    case other => other
  }

  /**
    * syntax アノテーションで指定された文法規則の情報を集めてくる関数
    * @param trees 対象とする構文木のリスト
    * @return 文法規則の情報のリスト
    */
  private def collectRules (trees: List[Tree]): List[SyntaxInfo] = trees.flatMap {
    case classDef : ClassDef  => classRule(classDef)
    case moduleDef: ModuleDef => objectRule(moduleDef)
    case fieldDef : ValDef    => fieldRule(fieldDef)
    case funDef   : DefDef    => functionRule(funDef)
    case _                    => Nil
  }

  /**
    * クラス定義に対応する文法規則の情報を返す関数
    * @param classDef クラス定義
    * @return プライマリコンストラクタに対応する文法規則 ++ 補助コンストラクタに対応する文法規則
    */
  private def classRule (classDef: ClassDef): List[SyntaxInfo] = {
    val ClassDef(mod, name, Nil, Template(_, _, body)) = classDef
    val constructors = findConstructors(body)
    val primaryRules = findPrimaryConstructor(body, constructors).map(primaryConstructorRules(mod, name, _)).getOrElse(Nil)
    val auxiliaryRules = constructors.flatMap(auxiliaryConstructorRules(name, _))
    primaryRules ++ auxiliaryRules
  }

  /**
    * シングルトンオブジェクト定義に対応する文法規則の情報を返す関数
    * @param moduleDef シングルトンオブジェクトの定義
    * @return オブジェクト参照に対応する文法規則の情報
    */
  private def objectRule (moduleDef: ModuleDef): List[SyntaxInfo] = {
    val ModuleDef(mod, name, _) = moduleDef
    operatorRule(mod, Nil, SingletonTypeTree(Ident(name)), _ => ObjectRef(name))
  }

  /**
    * フィールド定義に対応する文法規則の情報を返す関数
    * @param fieldDef フィールド定義
    * @return オブジェクト参照に対応する文法規則の情報
    */
  private def fieldRule (fieldDef: ValDef): List[SyntaxInfo] = {
    val ValDef(mod, name, fieldType, _) = fieldDef
    operatorRule(mod, Nil, fieldType, _ => ObjectRef(name))
  }

  /**
    * 関数に対応する文法規則の情報を返す関数
    * @param funDef 関数定義
    * @return 対応する文法規則の情報
    */
  private def functionRule (funDef: DefDef): List[SyntaxInfo] = {
    val DefDef(mod, name, Nil, paramLists, returnType, _) = funDef
    operatorRule(mod, paramLists, returnType, FunctionCall(name, _))
  }

  /**
    * 全てのコンストラクタ定義を返す関数
    * @param body クラス定義のボディ部分
    * @return 全てのコンストラクタ定義
    */
  private def findConstructors (body: List[Tree]): List[DefDef] = body.collect {
    case c @ DefDef(_, termNames.CONSTRUCTOR, _, _, _, _) => c
  }

  /**
    * プライマリコンストラクタを探す関数
    * @param body クラス定義のボディ部分
    * @param constructors 全てのコンストラクタ (bodyから検索可能だが、こうした方が定義がシンプルになる)
    * @return Option[プライマリコンストラクタ]
    */
  private def findPrimaryConstructor (body: List[Tree], constructors: List[DefDef]): Option[DefDef] = {
    val paramAccessors = body.collect {
      case ValDef(mods, name, _, _) if mods.hasFlag(Flag.PARAMACCESSOR) => name
    }
    constructors.find {
      case DefDef(_, termNames.CONSTRUCTOR, _, paramLists, _, _) => paramAccessors.forall { paramName =>
        paramLists.exists {
          _.exists { p => p.mods.hasFlag(Flag.PARAMACCESSOR) && p.name == paramName }
        }
      }
    }
  }

  /**
    * プライマリコンストラクタに対応する文法規則の情報を返す関数
    * @param mod クラス定義のアノテーションなどの修飾詞情報
    * @param typeName 型名
    * @param primaryConstructor プライマリコンストラクタ
    * @return 対応する文法規則の情報
    */
  private def primaryConstructorRules (mod: Modifiers, typeName: TypeName, primaryConstructor: DefDef): List[SyntaxInfo] = {
    val DefDef(_, termNames.CONSTRUCTOR, Nil, paramLists, _, _) = primaryConstructor
    operatorRule(mod, paramLists, Ident(typeName), ConstructorCall(typeName, _))
  }

  /**
    * 補助コンストラクタに対応する文法規則の情報を返す関数
    * @param typeName 型名
    * @param constructor 補助コンストラクタ
    * @return 対応する文法規則の情報
    */
  private def auxiliaryConstructorRules (typeName: TypeName, constructor: DefDef): List[SyntaxInfo] = {
    val DefDef(mod, termNames.CONSTRUCTOR, Nil, paramLists, _, _) = constructor
    operatorRule(mod, paramLists, Ident(typeName), ConstructorCall(typeName, _))
  }

  /**
    * syntaxアノテーションで指定された文法規則の情報を返す関数
    * @param mod アノテーションなどの修飾詞情報
    * @param paramLists 引数リストのリスト
    * @param returnType 返り値の型を表す構文木
    * @param semantics 対応する関数呼び出しを表現する
    * @return syntaxアノテーションで表現された文法規則の情報
    */
  private def operatorRule (mod: Modifiers, paramLists: List[List[ValDef]], returnType: Tree, semantics: (List[Tree] => List[List[Tree]]) => SemanticActionImpl): List[SyntaxInfo] = {
    val TypeWithSyntaxAnnotation(annotations, ret) = returnType

    ( findAnnotation("syntax", mod) ++ annotations ).flatten.map {
      syntax => syntaxAnnotationRule(syntax, paramLists, ret, semantics)
    }
  }

  /**
    * syntaxアノテーションに対応する文法規則の情報を返す関数
    * @param syntaxAnnotation syntax アノテーションの引数
    * @param paramLists 引数リストのリスト
    * @param returnType 返り値の型を表す構文木
    * @param semantics 対応する関数呼び出しを表現する
    * @return syntaxアノテーションに対応する文法規則の情報
    */
  private def syntaxAnnotationRule (syntaxAnnotation: Tree, paramLists: List[List[ValDef]], returnType: Tree, semantics: (List[Tree] => List[List[Tree]]) => SemanticActionImpl): SyntaxInfo = {
    syntaxAnnotation match {
      case Apply(Select(Apply(Ident(TermName("StringContext")), parts), TermName("s")), args) =>
        val operators = getOperators(parts)
        val (operands, correspondence) = getOperands(args, paramLists)
        SyntaxInfo(returnType, operators, operands, semantics(correspondence))

      case other =>
        c.abort(other.pos, s"""@syntax only supports s"..." for describing syntax: $other""")
    }
  }

  /**
    * StringContextの引数からオペレータ(キーワード)のリストを作る関数
    * @param parts StringContextの引数
    * @return オペレータ(キーワード)を表現する文字列のリスト
    */
  private def getOperators (parts: List[Tree]): List[List[String]] = parts.map {
    case Literal(Constant("")) => Nil
    case Literal(Constant(str: String)) => str.split(" ").filter(_ != "").toList
    case other => c.abort(other.pos, s"""@syntax only supports s"..." for describing syntax: $other""")
  }

  /**
    * オペランド名を解決する関数
    * @param args オペランド名のリスト
    * @param paramLists 引数リストのリスト
    * @return (オペランドの型を表現する構文木のリスト, オペランド列を引数リストのリストに直す関数)
    */
  private def getOperands (args: List[Tree], paramLists: List[List[ValDef]]): (List[Tree], List[Tree] => List[List[Tree]]) = {
    val parameters = for {
      (paramList, index1)                              <- paramLists.zipWithIndex
      (ValDef(_, valName, valType, EmptyTree), index2) <- paramList.zipWithIndex
    } yield valName -> (valType -> (index1 -> index2))

    val parameterMap = parameters.toMap[Name, (Tree, (Int, Int))]

    val operands = args.map { case Ident(termName) => parameterMap(termName) }

    // List[(operand, (index1, index2))] を作り, index1 でグループ化してソートし, 更に各グループ内で index2 でソートする
    val correspondence: List[Tree] => List[List[Tree]] = tree => {
      tree.zip(operands.map(_._2)).groupBy(_._2._1).toList.sortBy(_._1).map(_._2.sortBy(_._2._2).map(_._1))
    }

    (operands.map(_._1), correspondence)
  }
}
