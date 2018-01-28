package com.phenan.scalalr
package macroimpl

import shared._

import shapeless.Inl

trait SyntaxCollectorModule {
  this: MacroSyntaxRuleModule with SyntaxRuleModule with TypingModule with AnnotationFinderModule with MacroModule =>

  import c.universe._

  def collectSyntax (tree: Tree): SyntaxRule = new SyntaxCollector(tree).syntax

  private class SyntaxCollector (tree: Tree) {

    /**
      * dslアノテーションで指定されたobject定義の文法
      */
    def syntax: SyntaxRule = tree match {
      case ModuleDef (_, _, Template(_, _, body)) =>
        SyntaxRule(start, syntaxRules(body))
      case other =>
        c.abort(tree.pos, s"@dsl can be annotated to object: $other")
    }

    private lazy val typeChecker: TypeChecker = TypeChecker(tree)

    /**
      * dslアノテーションの型引数から開始記号を求める関数
      * @return 開始記号
      */
    private lazy val start: NonTerminal = c.prefix.tree match {
      case Apply(Select(New(AppliedTypeTree(Ident(TypeName("dsl")), List(t))), termNames.CONSTRUCTOR), Nil) => NonTerminalImpl(typeChecker.check(t))
      case _ => c.abort(c.prefix.tree.pos, "@dsl should take a type argument and should not take an argument")
    }

    /**
      * dslアノテーションで指定されたobjectのボディ部分から文法規則を生成する関数
      * @param body object定義のボディ部分
      * @return 文法規則
      */
    private def syntaxRules (body: List[Tree]): List[Rule] = {
      val rules = body.flatMap {
        case moduleDef: ModuleDef => objectRules(moduleDef)
        case fieldDef : ValDef    => fieldRules(fieldDef)
        case classDef : ClassDef  => classRules(classDef)
        case funDef   : DefDef    => functionRules(funDef)
        case _                    => Nil
      }
      val nonTerminals = collectNonTerminals(rules)
      removeVerboseRules(rules ++ literalRules(nonTerminals), nonTerminals)
    }

    /**
      * 冗長な文法規則を削除する関数
      * @param rules 文法規則
      * @param nonTerminals 非終端記号の集合
      * @return 与えられた非終端記号または開始記号を左辺とするような文法規則のリスト
      */
    private def removeVerboseRules (rules: List[Rule], nonTerminals: Set[NonTerminal]): List[Rule] = {
      rules.filter(rule => nonTerminals.contains(rule.left) || rule.left == start)
    }

    /**
      * 与えられた非終端記号に対応するリテラルを記述できる文法規則を生成する関数
      * @param nonTerminals 非終端記号の集合
      * @return 対応するリテラルの文法規則
      */
    private def literalRules (nonTerminals: Set[NonTerminal]): Set[Rule] = {
      nonTerminals.map(nt => Rule(nt, List(Symbol(Terminal(LiteralTokenImpl(nt.ntType)))), LiteralRef))
    }

    /**
      * 文法規則の右辺で利用されている全ての非終端記号の集合を求める関数
      * @param rules 文法規則
      * @return 非終端記号の集合
      */
    private def collectNonTerminals (rules: List[Rule]): Set[NonTerminal] = {
      rules.flatMap { _.right.collect { case Inl(nt) => nt } }.toSet
    }

    /**
      * シングルトンオブジェクト定義に対応する文法規則を生成する関数
      * @param moduleDef シングルトンオブジェクトの定義
      * @return オブジェクト参照に対応する文法規則 ++ 継承関係に対応する文法規則
      */
    private def objectRules (moduleDef: ModuleDef): List[Rule] = {
      val ModuleDef(mod, name, Template(parents, _, _)) = moduleDef
      val objectRefRules = buildOperatorRules(mod, Nil, SingletonTypeTree(Ident(name)), _ => ObjectRef(name))
      objectRefRules ++ inheritanceRules(SingletonTypeTree(Ident(name)), parents)
    }

    /**
      * フィールド定義に対応する文法規則を生成する関数
      * @param fieldDef フィールド定義
      * @return オブジェクト参照に対応する文法規則
      */
    private def fieldRules (fieldDef: ValDef): List[Rule] = {
      val ValDef(mod, name, fieldType, _) = fieldDef
      buildOperatorRules(mod, Nil, fieldType, _ => ObjectRef(name))
    }

    /**
      * クラス定義に対応する文法規則を生成する関数
      * @param classDef クラス定義
      * @return プライマリコンストラクタに対応する文法規則 ++ 補助コンストラクタに対応する文法規則 ++ 継承関係に対応する文法規則
      */
    private def classRules (classDef: ClassDef): List[Rule] = {
      val ClassDef(mod, name, Nil, Template(parents, _, body)) = classDef
      val constructors = findConstructors(body)
      val primaryRules = findPrimaryConstructor(body, constructors).map(primaryConstructorRules(mod, name, _)).getOrElse(Nil)
      val auxiliaryRules = constructors.flatMap(auxiliaryConstructorRules(name, _))
      primaryRules ++ auxiliaryRules ++ inheritanceRules(Ident(name), parents)
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
      * プライマリコンストラクタに対応する文法規則を生成する関数
      * @param mod クラス定義のアノテーションなどの修飾詞情報
      * @param typeName 型名
      * @param primaryConstructor プライマリコンストラクタ
      * @return 対応する文法規則
      */
    private def primaryConstructorRules (mod: Modifiers, typeName: TypeName, primaryConstructor: DefDef): List[Rule] = {
      val DefDef(_, termNames.CONSTRUCTOR, Nil, paramLists, _, _) = primaryConstructor
      buildOperatorRules(mod, paramLists, Ident(typeName), ConstructorCall(typeName, _))
    }

    /**
      * 補助コンストラクタに対応する文法規則を生成する関数
      * @param typeName 型名
      * @param constructor 補助コンストラクタ
      * @return 対応する文法規則
      */
    private def auxiliaryConstructorRules (typeName: TypeName, constructor: DefDef): List[Rule] = {
      val DefDef(mod, termNames.CONSTRUCTOR, Nil, paramLists, _, _) = constructor
      buildOperatorRules(mod, paramLists, Ident(typeName), ConstructorCall(typeName, _))
    }

    /**
      * 継承関係を文法規則に変換する関数
      * @param typeName 型名
      * @param parents スーパータイプ
      * @return 文法規則
      */
    private def inheritanceRules (typeName: Tree, parents: List[Tree]): List[Rule] = {
      val expr = List(Symbol(NonTerminalImpl(typeChecker.check(typeName))))
      parents.map { superType => Rule(NonTerminalImpl(typeChecker.check(superType)), expr, Inheritance) }
    }

    /**
      * 関数に対応する文法規則を生成する関数
      * @param functionDef 関数定義
      * @return 対応する文法規則
      */
    private def functionRules (functionDef: DefDef): List[Rule] = {
      val DefDef(mod, name, Nil, paramLists, returnType, _) = functionDef
      buildOperatorRules(mod, paramLists, returnType, FunctionCall(name, _))
    }

    /**
      * syntaxアノテーションで指定された文法規則を生成する関数
      * @param mod アノテーションなどの修飾詞情報
      * @param paramLists 引数リストのリスト
      * @param returnType 返り値の型を表す構文木
      * @param semantics 対応する関数呼び出しを表現する
      * @return syntaxアノテーションで表現された文法規則
      */
    private def buildOperatorRules (mod: Modifiers, paramLists: List[List[ValDef]], returnType: Tree, semantics: (List[Tree] => List[List[Tree]]) => SemanticActionImpl): List[Rule] = {
      val TypeWithSyntaxAnnotation(annotations, ret) = returnType

      ( findAnnotation("syntax", mod) ++ annotations ).flatten.map {
        syntax => buildRulesOfSyntaxAnnotation(syntax, paramLists, typeChecker.check(ret), semantics)
      }
    }

    /**
      * syntaxアノテーションに対応する文法規則を生成する関数
      * @param syntaxAnnotation syntax アノテーションの引数
      * @param paramLists 引数リストのリスト
      * @param returnType 返り値の型
      * @param semantics 対応する関数呼び出しを表現する
      * @return syntaxアノテーションに対応する文法規則
      */
    private def buildRulesOfSyntaxAnnotation (syntaxAnnotation: Tree, paramLists: List[List[ValDef]], returnType: GenericType, semantics: (List[Tree] => List[List[Tree]]) => SemanticActionImpl): Rule = {
      val (expr, paramIndexes) = buildSyntaxExpressionFromSyntaxAnnotation(syntaxAnnotation, paramLists)
      Rule(NonTerminalImpl(returnType), expr, semantics(paramIndexes))
    }

    /**
      * syntaxアノテーションから文法式とオペランドと引数の対応を出力する関数
      * @param syntaxAnnotation syntax アノテーションの引数
      * @param paramLists 引数リストのリスト
      * @return (文法式, オペランドと引数の対応)
      */
    private def buildSyntaxExpressionFromSyntaxAnnotation (syntaxAnnotation: Tree, paramLists: List[List[ValDef]]): (List[Symbol], List[Tree] => List[List[Tree]]) = {
      syntaxAnnotation match {
        case Apply(Select(Apply(Ident(TermName("StringContext")), parts), TermName("g")), args) =>
          val operators = buildOperatorListFromStringContext(parts)
          val operands = resolveOperands(args, paramLists)
          ( buildSyntaxExpression(operators, operands._1), operands._2 )
      }
    }

    /**
      * StringContextの引数からオペレータ(キーワード)のリストを作る関数
      * @param parts StringContextの引数
      * @return オペレータ(キーワード)のリスト
      */
    private def buildOperatorListFromStringContext (parts: List[Tree]): List[List[Keyword]] = parts.map {
      case Literal(Constant("")) => Nil
      case Literal(Constant(str: String)) => str.split(" ").filter(_ != "").map(Keyword).toList
      case other => c.abort(other.pos, s"""@syntax only supports g"..." for describing syntax: $other""")
    }

    /**
      * オペレータ(キーワード)のリストとオペランド(引数部)のリストから文法式を組み立てる
      * @param operators オペレータ(キーワード)のリスト
      * @param operands オペランド(引数部)のリスト
      * @return 文法式
      */
    private def buildSyntaxExpression (operators: List[List[Keyword]], operands: List[NonTerminal]): List[Symbol] = {
      def operatorSymbols = operators.map(_.map(k => Symbol(Terminal(k))))
      def operandSymbols = operands.map(nt => List(Symbol(nt)))
      ( Nil :: operandSymbols zip operatorSymbols ).flatMap ( pair => pair._1 ++ pair._2 )
    }

    /**
      * オペランド名を解決する関数
      * @param operands オペランド名のリスト
      * @param paramLists 引数リストのリスト
      * @return (非終端記号, オペランド列を引数リストのリストに直す関数)
      */
    private def resolveOperands (operands: List[Tree], paramLists: List[List[ValDef]]): (List[NonTerminal], List[Tree] => List[List[Tree]]) = {
      val parameters = for {
        (paramList, index1)                              <- paramLists.zipWithIndex
        (ValDef(_, valName, valType, EmptyTree), index2) <- paramList.zipWithIndex
      } yield valName -> (NonTerminalImpl(typeChecker.check(valType)) -> (index1 -> index2))

      val parameterMap = parameters.toMap[Name, (NonTerminal, (Int, Int))]

      val nonTerminals = operands.map { case Ident(termName) => parameterMap(termName) }

      // List[(Tree, (index1, index2))] を作り, index1 でグループ化してソートし, 更に各グループ内で index2 でソートする
      val correspondence: List[Tree] => List[List[Tree]] = tree => {
        tree.zip(nonTerminals.map(_._2)).groupBy(_._2._1).toList.sortBy(_._1).map(_._2.sortBy(_._2._2).map(_._1))
      }

      (nonTerminals.map(_._1), correspondence)
    }
  }
}
