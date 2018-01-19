package com.phenan.scalalr

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

    def literalIdentifier (lit: LiteralToken): String

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

    def moduleDefinition (moduleName: String, members: List[MemberDef]): MemberDef

    def branchDataTypeDef (nt: NonTerminal, superType: Option[NonTerminal]): MemberDef
    def derivationDateTypeDef (nt: NonTerminal, params: List[Parameter], superType: Option[NonTerminal]): MemberDef

    def caseClassDef (name: String, typeParams: List[TypeParameter], params: List[Parameter]): MemberDef
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

    def constructAST (nonTerminal: NonTerminal, args: List[Expr]): Expr
  }

  case class CodeGenerator (automaton: LALRAutomaton) {

    import output._

    def generateCode (module: MemberDef): GeneratedCode = generateProgram(List(module))
    def generateCode (modules: List[MemberDef]): GeneratedCode = generateProgram(modules)

    lazy val program: MemberDef = moduleDefinition ( automaton.syntax.name,
      nodeClassDefinitions ++ keywordObjectDefinitions ++ literalDSLDefinitions ++
      keywordDSLDefinitions ++ keywordTransitionDefinitions ++
      literalTransitionDefinitions ++
      shiftImplicitDefinitions ++ reduceImplicitDefinitions ++ acceptImplicitDefinitions
    )

    lazy val astDataTypeDefinitions: List[MemberDef] = automaton.syntax.nonTerminals.toList.flatMap { nt =>
      automaton.syntax.rules.find(_.left == nt).map {
        case BranchRule(_, _)         => branchDataTypeDef(nt, findSuperType(nt))
        case DerivationRule(_, right) => derivationDateTypeDef(nt, collectDerivationDataParameters(right), findSuperType(nt))
      }
    }

    private def findSuperType (nt: NonTerminal): Option[NonTerminal] = automaton.syntax.rules.collectFirst {
      case BranchRule(left, right) if right.contains(nt) => left
    }

    private def collectDerivationDataParameters (right: List[Symbol]): List[Parameter] = right.collect {
      case Inl(nt)            => nonTerminalType(nt)
      case Inr(Inl(Inl(lit))) => literalType(lit)
    }.zipWithIndex.map {
      case (t, n) => parameter(s"arg$n", t)
    }

    /**
      * LALR オートマトンの各ノードを表現するデータ型の定義を出力する関数
      */
    lazy val nodeClassDefinitions: List[MemberDef] = automaton.nodes.toList.map { node =>
      if (automaton.start == node) caseObjectDef(nodeName(node))
      else automaton.state(node) match {
        case Inl(nt)            => caseClassDef(nodeName(node), typeParameters("NX"), List(parameter("prev", simpleType("NX")), parameter("value", nonTerminalType(nt))))
        case Inr(Inl(Inl(lit))) => caseClassDef(nodeName(node), typeParameters("NX"), List(parameter("prev", simpleType("NX")), parameter("value", literalType(lit))))
        case _                  => caseClassDef(nodeName(node), typeParameters("NX"), List(parameter("prev", simpleType("NX"))))
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
    lazy val literalDSLDefinitions: List[MemberDef] = automaton.syntax.literals.toList.map { literal =>
      functionDef(literalIdentifier(literal),
                  Nil,
                  List(parameter("value", literalType(literal))),
                  Nil,
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

      val tokenListArgFunctionDef = functionDef(k.scalaIdent, List(typeParameter("U", simpleType("TokenList")), typeParameter("N3")), List(parameter("tokens", simpleType("U"))),
                                                List(parameter("transitions", transitionsType(simpleType("U"), simpleType("N2"), simpleType("N3")))),
                                                simpleType("N3"),
                                                objectRef("transitions").callTransit(objectRef(k.scalaIdent), objectRef("tokens")))

      List(implicitClassDef(typeParameters("T", "N1", "N2"), parameter("value", simpleType("T")),
                            List(parameter("transition1", transitionType(genericLiteralType("T"), startNodeType, simpleType("N1"))),
                                 parameter("transition2", transitionType(keywordType(k), simpleType("N1"), simpleType("N2")))),
                            List(functionDef(k.scalaIdent, Nil, Nil, Nil, simpleType("N2"),
                                             objectRef("transition2").callTransit(objectRef("transition1").callTransit(startNodeObjRef, constructLiteralObj(simpleType("T"), objectRef("value"))), keywordObjRef(k))),
                                 literalArgFunctionDef, tokenListArgFunctionDef)),
           implicitClassDef(List(typeParameter("T", simpleType("TokenList")), typeParameter("N1"), typeParameter("N2")), parameter("value", simpleType("T")),
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
    lazy val literalTransitionDefinitions: List[MemberDef] = automaton.syntax.literals.toList.map { lit =>
      val methodBody = objectRef("transition").callTransit(objectRef("node"), constructLiteralObj(literalType(lit), objectRef("value")))
      implicitClassDef(typeParameters("N1", "N2"), parameter("node", simpleType("N1")),
                       List(parameter("transition", transitionType(literalTokenTypes(lit), simpleType("N1"), simpleType("N2")))),
                       List(functionDef(literalIdentifier(lit), Nil, List(parameter("value", literalType(lit))), Nil, simpleType("N2"), methodBody)))
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
          implicitFunctionDef(typeParams, Nil, Nil, parameterizedType("Shift", List(terminalType(terminal), fromType, toType)),
                              callApply(objectRef("Shift"), List(terminalType(terminal), fromType, toType), List(bodyLambda)))
        }
      }
    }.toList

    /**
      * Reduce 操作を表す implicit value の定義
      */
    lazy val reduceImplicitDefinitions: List[MemberDef] = {
      automaton.reduce.toList.flatMap { case (from, (nonTerminal, expr, lookahead)) =>
        automaton.syntax.rules.collect {
          case BranchRule(left, right) if left == nonTerminal && expr.size == 1 && right.contains(Coproduct.unsafeGet(expr.head)) => for {
            via  <- automaton.reverseEdges(from)
            la   <- lookahead
            dest <- automaton.goTo(via).get(nonTerminal)
          } yield reduceBranch(from, via, dest, la)
          case DerivationRule(left, right) if left == nonTerminal && right == expr => for {
            path <- reducePath(from, expr)
            la   <- lookahead
            dest <- automaton.goTo(path.head).get(nonTerminal)
          } yield reduceDerivation(nonTerminal, path, dest, la)
        }.flatten
      }
    }

    /**
      * 終了処理を表す implicit value の定義
      */
    lazy val acceptImplicitDefinitions: List[MemberDef] = automaton.accept.toList.map { node =>
      val fromType = parameterizedType(nodeName(node), simpleTypes("NX"))
      implicitFunctionDef(typeParameters("NX"), Nil, Nil,
                          parameterizedType("Accept", List(fromType, nonTerminalType(automaton.syntax.start))),
                          callApply(objectRef("Accept"), List(fromType, nonTerminalType(automaton.syntax.start)),
                                    List(lambda(List(parameter("s", fromType)), fieldRef(objectRef("s"), "value")))))
    }

    /**
      * Reduce による巻き戻りの道のりを求める関数
      * @param from reduce の開始地点となる LR closure
      * @param expr reduce 対象の文法
      * @return Reduce による巻き戻りの道のりを表現する LR closure のリストの集合
      */
    private def reducePath (from: LRClosure, expr: List[Symbol]) = expr.foldRight(Set(List(from))) { (symbol, set) =>
      for {
        path <- set if automaton.state(path.head) == symbol
        node <- automaton.reverseEdges(path.head)
      } yield node :: path
    }

    private def reduceBranch (departure: LRClosure, via: LRClosure, destination: LRClosure, lookahead: Terminal): MemberDef = {
      val (typeParams, baseType) =
        if (automaton.start == via) (Nil, startNodeType)
        else (typeParameters("NX"), parameterizedType(nodeName(via), simpleTypes("NX")))

      val fromType = parameterizedType(nodeName(departure), List(baseType))
      val toType = parameterizedType(nodeName(destination), List(baseType))

      val bodyLambda = lambda(List(parameter("s", fromType)),
                              callApply(objectRef(nodeName(destination)), List(baseType),
                                        List(fieldRef(objectRef("s"), "prev"), fieldRef(objectRef("s"), "value"))))

      implicitFunctionDef(typeParams, Nil, Nil, parameterizedType("Reduce", List(terminalType(lookahead), fromType, toType)),
                          callApply(objectRef("Reduce"), List(terminalType(lookahead), fromType, toType), List(bodyLambda)))
    }

    private def reduceDerivation (left: NonTerminal, path: List[LRClosure], destination: LRClosure, lookahead: Terminal): MemberDef = {
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
                                        List(prevField, constructAST(left, astElements))))

      implicitFunctionDef(typeParams, Nil, Nil, parameterizedType("Reduce", List(terminalType(lookahead), fromType, toType)),
                          callApply(objectRef("Reduce"), List(terminalType(lookahead), fromType, toType), List(bodyLambda)))
    }

    /**
      * 終端記号に対応する型を求める関数
      * @param t 終端記号
      * @return 型
      */
    private def terminalType (t: Terminal): Type = t match {
      case Inl(lit)    => literalTokenTypes(lit)
      case Inr(Inl(k)) => objectType(keywordTokenTypeNames(k))
      case Inr(Inr(_)) => objectType("EoI")
    }

    /**
      * リテラルとそのリテラルの内部型のマッピング
      * LALR オートマトンの開始ノードからそのリテラルのエッジが出ている場合、そのエッジが指すノードをリテラルの型として用いる
      */
    private lazy val literalTokenTypes: Map[LiteralToken, Type] = automaton.syntax.literals.map { literal =>
      literal -> parameterizedType("Literal", List(literalType(literal)))
    }.toMap

    private def genericLiteralType (name: String): Type = parameterizedType("Literal", simpleTypes(name))

    private def constructLiteralObj (litType: Type, arg: Expr): Expr = callApply(objectRef("Literal"), List(litType), List(arg))

    private def transitionType(terminal: Type, from: Type, to: Type): Type = parameterizedType("Transition", List(terminal, from, to))

    private def transitionsType(tokens: Type, from: Type, to: Type): Type = parameterizedType("Transitions", List(tokens, from, to))

    private def tokenListType(tokenTypes: Type*): Type = tokenListType(tokenTypes.toList)

    private def tokenListType(tokenTypes: List[Type]): Type = tokenTypes match {
      case head :: tail => parameterizedType("TokenListCons", List(head, tokenListType(tail)))
      case Nil          => simpleType("TokenListSentinel")
    }

    private def singleTokenListObj (tokenType: Type, token: Expr): Expr = {
      callApply(objectRef("TokenListCons"), List(tokenType, simpleType("TokenListSentinel")), List(token, objectRef("TokenListSentinel")))
    }

    private def keywordType (k: Keyword): Type = objectType(keywordTokenTypeNames(k))

    private def keywordObjRef (k: Keyword): Expr = objectRef(keywordTokenTypeNames(k))

    private lazy val startNodeType: Type = objectType(startNode)

    private lazy val startNodeObjRef: Expr = objectRef(startNode)

    private implicit class ExprOps (e: Expr) {
      def callTransit (from: Expr, token: Expr): Expr = methodCall(e, "transit", Nil, List(from, token))
    }

    private lazy val keywordTokenTypeNames: Map[Keyword, String] = automaton.syntax.keywords.map(_ -> generateUniqueName).toMap
    private lazy val nodeName: Map[LRClosure, String] = automaton.nodes.map(_ -> generateUniqueName).toMap

    private lazy val startNode: String = nodeName(automaton.start)
  }
}
