package com.phenan.scalalr.macroimpl

trait SyntaxInfoCollectorModule {
  this: AnnotationFinderModule with SyntaxInfoModule with CommonNamesModule with MacroUtilitiesModule with MacroModule =>

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
    val constructors = MacroUtilities.findConstructors(body)
    val primaryRules = MacroUtilities.findPrimaryConstructor(body).map(primaryConstructorRules(mod, name, _)).getOrElse(Nil)
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
    operatorRule(mod, Nil, SingletonTypeTree(Ident(name)), _ => SemanticActionImpl.returnConstant(q"$name"))
  }

  /**
    * フィールド定義に対応する文法規則の情報を返す関数
    * @param fieldDef フィールド定義
    * @return オブジェクト参照に対応する文法規則の情報
    */
  private def fieldRule (fieldDef: ValDef): List[SyntaxInfo] = {
    val ValDef(mod, name, fieldType, _) = fieldDef
    operatorRule(mod, Nil, fieldType, _ => SemanticActionImpl.returnConstant(q"$name"))
  }

  /**
    * 関数に対応する文法規則の情報を返す関数
    * @param funDef 関数定義
    * @return 対応する文法規則の情報
    */
  private def functionRule (funDef: DefDef): List[SyntaxInfo] = {
    val DefDef(mod, name, Nil, paramLists, returnType, _) = funDef
    operatorRule(mod, paramLists, returnType, SemanticActionImpl.functionCall(q"$name", _))
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
    operatorRule(mod, paramLists, Ident(typeName), SemanticActionImpl.constructorCall(tq"$typeName", _))
  }

  /**
    * 補助コンストラクタに対応する文法規則の情報を返す関数
    * @param typeName 型名
    * @param constructor 補助コンストラクタ
    * @return 対応する文法規則の情報
    */
  private def auxiliaryConstructorRules (typeName: TypeName, constructor: DefDef): List[SyntaxInfo] = {
    val DefDef(mod, termNames.CONSTRUCTOR, Nil, paramLists, _, _) = constructor
    operatorRule(mod, paramLists, Ident(typeName), SemanticActionImpl.constructorCall(tq"$typeName", _))
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

    ( findSyntaxAnnotation(mod) ++ annotations ).flatten.flatMap { syntax =>
      val (operandLists, ss) = variableParameterRules(paramLists)
      syntaxAnnotationRule(syntax, operandLists, ret, semantics) :: ss
    }
  }

  private def variableParameterRules (paramLists: List[List[ValDef]]): (List[List[Operand]], List[SyntaxInfo]) = {
    paramLists.foldRight[(List[List[Operand]], List[SyntaxInfo])]((Nil, Nil)) {
      case (paramList, (operandLists, syntax)) =>
        val (operands, ss) = variableParameterRules_paramList(paramList)
        (operands :: operandLists, ss ++ syntax)
    }
  }

  private def variableParameterRules_paramList (paramList: List[ValDef]): (List[Operand], List[SyntaxInfo]) = {
    paramList.foldRight[(List[Operand], List[SyntaxInfo])]((Nil, Nil)) {
      case (ValDef(_, name, AppliedTypeTree(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TypeName("<repeated>")), List(TypeWithSepAnnotation(sep, t))), _), (params, syntax)) =>
        (RepOperand(name, t) :: params, variableParameterSyntax(t, sep) ++ syntax)
      case (ValDef(_, name, t, _), (params, syntax)) =>
        (NormalOperand(name, t) :: params, syntax)
    }
  }

  private def variableParameterSyntax (componentType: Tree, sep: Option[String]): List[SyntaxInfo] = {
    List(SyntaxInfo.epsilonOperator(seqTypeTreeOf(componentType), getNilListOf(componentType)),
         SyntaxInfo.unaryOperator(seqTypeTreeOf(componentType), Nil, componentType, Nil, arg => makeSingleElementList(componentType, arg)),
         SyntaxInfo.binaryOperator(seqTypeTreeOf(componentType), Nil, componentType, Nil, seqTailTypeTreeOf(componentType), Nil, (x, xs) => consSeq(x, seqTailToSeq(xs))),
         SyntaxInfo.unaryOperator(seqTailTypeTreeOf(componentType), sep.toList, componentType, Nil, arg => makeSingleElementSeqTail(componentType, arg)),
         SyntaxInfo.binaryOperator(seqTailTypeTreeOf(componentType), sep.toList, componentType, Nil, seqTailTypeTreeOf(componentType), Nil, (x, xs) => consSeqTail(x, xs)))
  }

  /**
    * syntaxアノテーションに対応する文法規則の情報を返す関数
    * @param syntaxAnnotation syntax アノテーションの引数
    * @param operandLists 引数リストのリスト
    * @param returnType 返り値の型を表す構文木
    * @param semantics 対応する関数呼び出しを表現する
    * @return syntaxアノテーションに対応する文法規則の情報
    */
  private def syntaxAnnotationRule (syntaxAnnotation: Tree, operandLists: List[List[Operand]], returnType: Tree, semantics: (List[Tree] => List[List[Tree]]) => SemanticActionImpl): SyntaxInfo = {
    syntaxAnnotation match {
      case Apply(Select(Apply(Ident(TermName("StringContext")), parts), TermName("s")), args) =>
        val operators = getOperators(parts)
        val (operands, correspondence) = getOperands(args, operandLists)
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
    * @param operandLists 引数リストのリスト
    * @return (オペランドの型を表現する構文木のリスト, オペランド列を引数リストのリストに直す関数)
    */
  private def getOperands (args: List[Tree], operandLists: List[List[Operand]]): (List[Tree], List[Tree] => List[List[Tree]]) = {
    val parameters = for {
      (operandList, index1) <- operandLists.zipWithIndex
      (operand, index2)     <- operandList.zipWithIndex
    } yield operand.name -> (operand -> (index1 -> index2))

    val parameterMap = parameters.toMap[Name, (Operand, (Int, Int))]

    val operands = args.map { case Ident(termName) => parameterMap(termName) }

    // List[(arg, index1, index2)] を作り, index1 でグループ化してソートし, 更に各グループ内で index2 でソートする
    val correspondence: List[Tree] => List[List[Tree]] = tree => {
      val zipped = tree.zip(operands).map {
        case (arg, (RepOperand(_, _), (i1, i2)))    => (q"$arg:_*", i1, i2)
        case (arg, (NormalOperand(_, _), (i1, i2))) => (arg, i1, i2)
      }
      zipped.groupBy(_._2).toList.sortBy(_._1).map(_._2.sortBy(_._3).map(_._1))
    }

    (operands.map(_._1.valType), correspondence)
  }

  private sealed trait Operand {
    def name: TermName
    def valType: Tree
  }

  private case class NormalOperand (name: TermName, valType: Tree) extends Operand

  private case class RepOperand (name: TermName, componentType: Tree) extends Operand {
    override def valType: Tree = seqTypeTreeOf(componentType)
  }
}
