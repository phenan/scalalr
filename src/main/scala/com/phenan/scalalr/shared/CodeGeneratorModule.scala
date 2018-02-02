package com.phenan.scalalr.shared

import shapeless._

trait CodeGeneratorModule {
  self: SyntaxRuleModule with LALRAutomatonModule =>

  import HList.ListCompat._

  val output: Output

  type GeneratedCode

  trait Output {
    type Type

    type MemberDef

    type Parameter

    type TypeParameter

    type Expr

    def generateProgram (modules: List[MemberDef]): GeneratedCode

    def generateUniqueName: String

    def literalIdentifier (lit: LiteralToken): Option[String]

    def simpleType (typeName: String): Type
    def objectType (objectName: String): Type

    def nonTerminalType (nt: NonTerminal): Type
    def literalType (lit: LiteralToken): Type

    def simpleTypes (names: String*): List[Type] = names.map(simpleType).toList

    def tuple2Type (v1: Type, v2: Type): Type
    def functionType (left: Type, right: Type): Type
    def parameterizedType (genName: String, args: List[Type]): Type

    def parameter (name: String, paramType: Type): Parameter
    def unusedParameter (paramType: Type): Parameter

    def typeParameter (name: String): TypeParameter
    def typeParameter (name: String, bound: Type): TypeParameter

    def typeParameters (names: String*): List[TypeParameter] = names.map(typeParameter).toList

    def sealedTraitDef (name: String, superType: Option[Type]): MemberDef

    def caseClassDef (name: String, typeParams: List[TypeParameter], params: List[Parameter], superType: Option[Type]): MemberDef
    def caseObjectDef (name: String): MemberDef

    def lazyValDef (name: String, valType: Type, value: Expr): MemberDef
    def functionDef (name: String, typeParams: List[TypeParameter], parameters: List[Parameter], implicitParams: List[Parameter], returnType: Type, body: Expr): MemberDef
    def implicitFunctionDef (typeParams: List[TypeParameter], parameters: List[Parameter], implicitParams: List[Parameter], returnType: Type, body: Expr): MemberDef

    def implicitClassDef (typeParams: List[TypeParameter], parameter: Parameter, implicitParams: List[Parameter], members: List[MemberDef]): MemberDef

    def objectRef (objectName: String): Expr
    def methodCall (receiver: Expr, methodName: String, typeArgs: List[Type], args: List[Expr]): Expr
    def fieldRef (receiver: Expr, fieldName: String): Expr
    def callApply (receiver: Expr, typeArgs: List[Type], args: List[Expr]): Expr
    def lambda (parameters: List[Parameter], body: Expr): Expr

    def constructAST (rule: Rule, args: List[Expr]): Expr
  }

  case class CodeGenerator (automaton: LALRAutomaton) {

    import output._

    def generateCode (module: MemberDef): GeneratedCode = generateProgram(List(module))
    def generateCode (modules: List[MemberDef]): GeneratedCode = generateProgram(modules)

    lazy val generatedDefinitions: List[MemberDef] = {
      nodeClassDefinitions ++ keywordObjectDefinitions ++ literalDSLDefinitions ++
      keywordDSLDefinitions ++ keywordTransitionDefinitions ++
      literalTransitionDefinitions ++
      shiftImplicitDefinitions ++ reduceImplicitDefinitions ++ acceptImplicitDefinitions
    }

    /**
      * LALR オートマトンの各ノードを表現するデータ型の定義を出力する関数
      */
    lazy val nodeClassDefinitions: List[MemberDef] = automaton.nodes.filterNot(automaton.start == _).toList.map { node =>
      automaton.state(node) match {
        case Inl(nt)            => caseClassDef(nodeName(node), typeParameters("NX"), List(parameter("prev", simpleType("NX")), parameter("value", nonTerminalType(nt))), None)
        case Inr(Inl(Inl(lit))) => caseClassDef(nodeName(node), typeParameters("NX"), List(parameter("prev", simpleType("NX")), parameter("value", literalType(lit))), None)
        case _                  => caseClassDef(nodeName(node), typeParameters("NX"), List(parameter("prev", simpleType("NX"))), None)
      }
    }

    /**
      * キーワード(オペレータ)を表現するオブジェクトの定義を出力する関数
      */
    lazy val keywordObjectDefinitions: List[MemberDef] = automaton.syntax.keywords.toList.map { k =>
      caseObjectDef(keywordTokenTypeNames(k))
    }

    /**
      * リテラルを表現するDSL関数の定義
      */
    lazy val literalDSLDefinitions: List[MemberDef] = for {
      literal <- automaton.syntax.literals.toList
      id <- literalIdentifier(literal)
    } yield {
      functionDef(id, Nil, List(parameter("value", literalType(literal))), Nil,
                  tokenListType(literalTokenTypes(literal)),
                  singleTokenListObj(literalTokenTypes(literal), constructLiteralObj(literalType(literal), objectRef("value"))))
    }

    /**
      * キーワード(オペレータ)を表すDSL関数の定義
      */
    lazy val keywordDSLDefinitions: List[MemberDef] = automaton.syntax.keywords.toList.map { k =>
      lazyValDef(k.scalaIdent, tokenListType(keywordType(k)),
                 singleTokenListObj(keywordType(k), keywordObjRef(k)))
    }

    /**
      * DSL のキーワードによる遷移を表現する関数の定義
      */
    lazy val keywordTransitionDefinitions: List[MemberDef] = automaton.syntax.keywords.toList.flatMap { k =>
      val literalArgFunctionDef = functionDef(k.scalaIdent, typeParameters("U", "N3"), List(parameter("literal", simpleType("U"))),
                                              List(parameter("transition", transitionType(genericLiteralType("U"), simpleType("N2"), simpleType("N3")))),
                                              simpleType("N3"),
                                              objectRef("transition").callTransit(objectRef(k.scalaIdent), constructLiteralObj(simpleType("U"), objectRef("literal"))))

      val tokenListArgFunctionDef = functionDef(k.scalaIdent, List(typeParameter("U", genericTokenListType), typeParameter("N3")), List(parameter("tokens", simpleType("U"))),
                                                List(parameter("transitions", transitionsType(simpleType("U"), simpleType("N2"), simpleType("N3")))),
                                                simpleType("N3"),
                                                objectRef("transitions").callTransit(objectRef(k.scalaIdent), objectRef("tokens")))

      List(implicitClassDef(typeParameters("T", "N1", "N2"), parameter("value", simpleType("T")),
                            List(parameter("transition1", transitionType(genericLiteralType("T"), startNodeType, simpleType("N1"))),
                                 parameter("transition2", transitionType(keywordType(k), simpleType("N1"), simpleType("N2")))),
                            List(functionDef(k.scalaIdent, Nil, Nil, Nil, simpleType("N2"),
                                             objectRef("transition2").callTransit(objectRef("transition1").callTransit(startNodeObjRef, constructLiteralObj(simpleType("T"), objectRef("value"))), keywordObjRef(k))),
                                 literalArgFunctionDef, tokenListArgFunctionDef)),
           implicitClassDef(List(typeParameter("T", genericTokenListType), typeParameter("N1"), typeParameter("N2")), parameter("value", simpleType("T")),
                            List(parameter("transition1", transitionsType(simpleType("T"), startNodeType, simpleType("N1"))),
                                 parameter("transition2", transitionType(keywordType(k), simpleType("N1"), simpleType("N2")))),
                            List(functionDef(k.scalaIdent, Nil, Nil, Nil, simpleType("N2"),
                                             objectRef("transition2").callTransit(objectRef("transition1").callTransit(startNodeObjRef, objectRef("value")), keywordObjRef(k))),
                                 literalArgFunctionDef, tokenListArgFunctionDef)),
           implicitClassDef(typeParameters("N1", "N2"), parameter("node", simpleType("N1")),
                            List(parameter("transition1", transitionType(keywordType(k), simpleType("N1"), simpleType("N2")))),
                            List(functionDef(k.scalaIdent, Nil, Nil, Nil, simpleType("N2"),
                                             objectRef("transition1").callTransit(objectRef("node"), keywordObjRef(k))),
                                 literalArgFunctionDef, tokenListArgFunctionDef)))
    }

    /**
      * DSL のリテラルによる遷移を表現する関数の定義
      */
    lazy val literalTransitionDefinitions: List[MemberDef] = for {
      lit <- automaton.syntax.literals.toList
      id  <- literalIdentifier(lit)
    } yield {
      val methodBody = objectRef("transition").callTransit(objectRef("node"), constructLiteralObj(literalType(lit), objectRef("value")))
      implicitClassDef(typeParameters("N1", "N2"), parameter("node", simpleType("N1")),
                       List(parameter("transition", transitionType(literalTokenTypes(lit), simpleType("N1"), simpleType("N2")))),
                       List(functionDef(id, Nil, List(parameter("value", literalType(lit))), Nil, simpleType("N2"), methodBody)))
    }

    /**
      * Shift 操作を表す implicit value の定義
      */
    lazy val shiftImplicitDefinitions: List[MemberDef] = {
      automaton.shift.flatMap { case (from, map) =>
        val (typeParams, fromType) =
          if (automaton.start == from) (Nil, startNodeType)
          else (typeParameters("NX"), parameterizedType(nodeName(from), simpleTypes("NX")))

        map.map { case (terminal, to) =>
          val toType = parameterizedType(nodeName(to), List(fromType))
          val bodyLambda = terminal match {
            case Inl(lit) => lambda(List(parameter("s", fromType), parameter("t", literalTokenTypes(lit))),
                                    callApply(objectRef(nodeName(to)), List(fromType), List(objectRef("s"), fieldRef(objectRef("t"), "value"))))
            case _        => lambda(List(parameter("s", fromType), unusedParameter(terminalType(terminal))),
                                    callApply(objectRef(nodeName(to)), List(fromType), List(objectRef("s"))))
          }
          implicitFunctionDef(typeParams, Nil, Nil, shiftType(terminalType(terminal), fromType, toType),
                              constructShiftObj(terminalType(terminal), fromType, toType, bodyLambda))
        }
      }
    }.toList

    /**
      * Reduce 操作を表す implicit value の定義
      */
    lazy val reduceImplicitDefinitions: List[MemberDef] = for {
      (from, (rule, lookahead)) <- automaton.reduce.toList
      path <- reducePath(from, rule)
      la   <- lookahead
      dest <- automaton.goTo(path.head).get(rule.left)
    } yield reduceImplicitDefinition(rule, path, dest, la)

    /**
      * 終了処理を表す implicit value の定義
      */
    lazy val acceptImplicitDefinitions: List[MemberDef] = for {
      (node, rule) <- automaton.accept.toList
      path         <- reducePath(node, rule)
    } yield acceptImplicitDefinition(rule, path)

    /**
      * Reduce による巻き戻りの道のりを求める関数
      * @param from reduce の開始地点となる LR closure
      * @param rule reduce 対象の文法
      * @return Reduce による巻き戻りの道のりを表現する LR closure のリストの集合
      */
    private def reducePath (from: LRClosure, rule: Rule) = rule.right.foldRight(Set(List(from))) { (symbol, set) =>
      for {
        path <- set if automaton.state(path.head) == symbol
        node <- automaton.reverseEdges(path.head)
      } yield node :: path
    }

    private def reduceImplicitDefinition (rule: Rule, path: List[LRClosure], destination: LRClosure, lookahead: Terminal): MemberDef = {
      val (typeParams, baseType) =
        if (automaton.start == path.head) (Nil, startNodeType)
        else (typeParameters("NX"), parameterizedType(nodeName(path.head), simpleTypes("NX")))

      val fromType = path.tail.foldLeft(baseType) { (arg, node) => parameterizedType(nodeName(node), List(arg)) }
      val toType = parameterizedType(nodeName(destination), List(baseType))

      val (prevField, astElements) = path.tail.foldRight[(Expr, List[Expr])]((objectRef("s"), Nil)) { case (node, (cur, args)) =>
        automaton.state(node) match {
          case Inl(_) | Inr(Inl(Inl(_))) => (fieldRef(cur, "prev"), fieldRef(cur, "value") :: args)    // 有意な引数
          case _                         => (fieldRef(cur, "prev"), args)                              // キーワード等は値に意味がない
        }
      }
      val bodyLambda = lambda(List(parameter("s", fromType)),
                              callApply(objectRef(nodeName(destination)), List(baseType),
                                        List(prevField, constructAST(rule, astElements))))

      implicitFunctionDef(typeParams, Nil, Nil, reduceType(terminalType(lookahead), fromType, toType),
                          constructReduceObj(terminalType(lookahead), fromType, toType, bodyLambda))
    }

    private def acceptImplicitDefinition (rule: Rule, path: List[LRClosure]): MemberDef = {
      val (typeParams, baseType) =
        if (automaton.start == path.head) (Nil, startNodeType)
        else (typeParameters("NX"), parameterizedType(nodeName(path.head), simpleTypes("NX")))

      val fromType = path.tail.foldLeft(baseType) { (arg, node) => parameterizedType(nodeName(node), List(arg)) }

      val astElements = path.tail.foldRight[(Expr, List[Expr])]((objectRef("s"), Nil)) { case (node, (cur, args)) =>
        automaton.state(node) match {
          case Inl(_) | Inr(Inl(Inl(_))) => (fieldRef(cur, "prev"), fieldRef(cur, "value") :: args)    // 有意な引数
          case _                         => (fieldRef(cur, "prev"), args)                              // キーワード等は値に意味がない
        }
      }._2

      val bodyLambda = lambda(List(parameter("s", fromType)), constructAST(rule, astElements))

      implicitFunctionDef(typeParams, Nil, Nil, acceptType(fromType, nonTerminalType(automaton.syntax.start)),
                          constructAcceptObj(fromType, nonTerminalType(automaton.syntax.start), bodyLambda))
    }

    /**
      * 終端記号に対応する型を求める関数
      * @param t 終端記号
      * @return 型
      */
    private def terminalType (t: Terminal): Type = t match {
      case Inl(lit)    => literalTokenTypes(lit)
      case Inr(Inl(k)) => objectType(keywordTokenTypeNames(k))
      case Inr(Inr(_)) => objectType("com.phenan.scalalr.internal.EoI")
    }

    /**
      * リテラルとそのリテラルの内部型のマッピング
      * LALR オートマトンの開始ノードからそのリテラルのエッジが出ている場合、そのエッジが指すノードをリテラルの型として用いる
      */
    private lazy val literalTokenTypes: Map[LiteralToken, Type] = automaton.syntax.literals.map { literal =>
      literal -> parameterizedType("com.phenan.scalalr.internal.Literal", List(literalType(literal)))
    }.toMap

    private def genericLiteralType (name: String): Type = parameterizedType("com.phenan.scalalr.internal.Literal", simpleTypes(name))

    private def constructLiteralObj (litType: Type, arg: Expr): Expr = callApply(objectRef("com.phenan.scalalr.internal.Literal"), List(litType), List(arg))

    private def constructShiftObj (terminal: Type, from: Type, to: Type, body: Expr): Expr = callApply(objectRef("com.phenan.scalalr.internal.Shift"), List(terminal, from, to), List(body))

    private def constructReduceObj (terminal: Type, from: Type, to: Type, body: Expr): Expr = callApply(objectRef("com.phenan.scalalr.internal.Reduce"), List(terminal, from, to), List(body))

    private def constructAcceptObj (from: Type, to: Type, body: Expr): Expr = callApply(objectRef("com.phenan.scalalr.internal.Accept"), List(from, to), List(body))

    private def shiftType (terminal: Type, from: Type, to: Type): Type = parameterizedType("com.phenan.scalalr.internal.Shift", List(terminal, from, to))

    private def reduceType (terminal: Type, from: Type, to: Type): Type = parameterizedType("com.phenan.scalalr.internal.Reduce", List(terminal, from, to))

    private def acceptType (from: Type, to: Type): Type = parameterizedType("com.phenan.scalalr.internal.Accept", List(from, to))

    private def transitionType (terminal: Type, from: Type, to: Type): Type = parameterizedType("com.phenan.scalalr.internal.Transition", List(terminal, from, to))

    private def transitionsType (tokens: Type, from: Type, to: Type): Type = parameterizedType("com.phenan.scalalr.internal.Transitions", List(tokens, from, to))

    private def genericTokenListType: Type = simpleType("com.phenan.scalalr.internal.TokenList")

    private def tokenListType(tokenTypes: Type*): Type = tokenListType(tokenTypes.toList)

    private def tokenListType(tokenTypes: List[Type]): Type = tokenTypes match {
      case head :: tail => parameterizedType("com.phenan.scalalr.internal.TokenListCons", List(head, tokenListType(tail)))
      case Nil          => simpleType("com.phenan.scalalr.internal.TokenListSentinel")
    }

    private def singleTokenListObj (tokenType: Type, token: Expr): Expr = {
      callApply(objectRef("com.phenan.scalalr.internal.TokenListCons"), List(tokenType, simpleType("com.phenan.scalalr.internal.TokenListSentinel")), List(token, objectRef("com.phenan.scalalr.internal.TokenListSentinel")))
    }

    private def keywordType (k: Keyword): Type = objectType(keywordTokenTypeNames(k))

    private def keywordObjRef (k: Keyword): Expr = objectRef(keywordTokenTypeNames(k))

    private lazy val startNodeType: Type = objectType(startNode)

    private lazy val startNodeObjRef: Expr = objectRef(startNode)

    private implicit class ExprOps (e: Expr) {
      def callTransit (from: Expr, token: Expr): Expr = methodCall(e, "transit", Nil, List(from, token))
    }

    private lazy val keywordTokenTypeNames: Map[Keyword, String] = automaton.syntax.keywords.map(_ -> generateUniqueName).toMap
    private lazy val nodeName: Map[LRClosure, String] = automaton.nodes.map { node =>
      if (automaton.start == node) node -> "com.phenan.scalalr.internal.StartNode"
      else node -> generateUniqueName
    }.toMap

    private lazy val startNode: String = nodeName(automaton.start)
  }
}
