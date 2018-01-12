package com.phenan.scalalr

import shapeless._

trait CodeGeneratorModule {
  self: SyntaxRuleModule with LALRAutomatonModule =>

  val output: Output

  type GeneratedCode

  trait Output {
    type Type

    type MemberDef

    type Parameter

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

    def moduleDefinition (moduleName: String, members: List[MemberDef]): MemberDef

    def branchDataTypeDef (nt: NonTerminal, superType: Option[NonTerminal]): MemberDef
    def derivationDateTypeDef (nt: NonTerminal, params: List[Parameter], superType: Option[NonTerminal]): MemberDef

    def caseClassDef (name: String, typeParams: List[String], params: List[Parameter]): MemberDef
    def caseObjectDef (name: String): MemberDef

    def lazyValDef (name: String, valType: Type, value: Expr): MemberDef
    def functionDef (name: String, typeParams: List[String], parameters: List[Parameter], implicitParams: List[Parameter], returnType: Type, body: Expr): MemberDef
    def implicitFunctionDef (typeParams: List[String], parameters: List[Parameter], implicitParams: List[Parameter], returnType: Type, body: Expr): MemberDef

    def implicitClassDef (typeParams: List[String], parameter: Parameter, implicitParams: List[Parameter], members: List[MemberDef]): MemberDef

    def objectRef (objectName: String): Expr
    def methodCall (receiver: Expr, methodName: String, typeArgs: List[Type], args: List[Expr]): Expr
    def fieldRef (receiver: Expr, fieldName: String): Expr
    def callApply (receiver: Expr, typeArgs: List[Type], args: List[Expr]): Expr
    def lambda (parameters: List[Parameter], body: Expr): Expr

    def constructAST (nonTerminal: NonTerminal, args: List[Expr]): Expr
  }

  case class CodeGenerator (automaton: LALRAutomaton) {

    import output._

    def generatedCodeWithDataDefinitions: GeneratedCode = generateProgram(List(programWithDataDefinitions))
    def generatedCodeWithoutDataDefinitions: GeneratedCode = generateProgram(List(program))
    def generatedCodeOfASTDataTypeDefinitions: GeneratedCode = generateProgram(astDataTypeDefinitions)

    lazy val program: MemberDef = moduleDefinition ( automaton.syntax.name,
      baseClassTypeDefinitions ++ nodeClassDefinitions ++ literalTokenTypeDefinitions ++ List(eoiObjectDefinition) ++
      keywordObjectDefinitions ++ literalDSLDefinitions ++ List(eoiDSLDefinition) ++ keywordDSLDefinitions ++
      basicImplicits ++ keywordTransitionDefinitions ++ literalTransitionDefinitions ++
      shiftImplicitDefinitions ++ reduceImplicitDefinitions ++ acceptImplicitDefinitions
    )

    lazy val programWithDataDefinitions: MemberDef = moduleDefinition ( automaton.syntax.name,
      astDataTypeDefinitions ++
      baseClassTypeDefinitions ++ nodeClassDefinitions ++ literalTokenTypeDefinitions ++ List(eoiObjectDefinition) ++
      keywordObjectDefinitions ++ literalDSLDefinitions ++ List(eoiDSLDefinition) ++ keywordDSLDefinitions ++
      basicImplicits ++ keywordTransitionDefinitions ++ literalTransitionDefinitions ++
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
      * shift, reduce, accept, transition を表現する型クラスの定義を出力する関数
      *  case class Shift [T, N1, N2] (shift: (N1, T) => N2)
      *  case class Reduce [T, N1, N2] (reduce: N1 => N2)
      *  case class Accept [NX, R] (accept: NX => R)
      *  case class Transition [T, N1, N2] (transit: (N1, T) => N2)
      */
    lazy val baseClassTypeDefinitions: List[MemberDef] = List (
      caseClassDef("Shift", List("T", "N1", "N2"), List(parameter("shift", functionType(tuple2Type(simpleType("N1"), simpleType("T")), simpleType("N2"))))),
      caseClassDef("Reduce", List("T", "N1", "N2"), List(parameter("reduce", functionType(simpleType("N1"), simpleType("N2"))))),
      caseClassDef("Accept", List("NX", "R"), List(parameter("accept", functionType(simpleType("NX"), simpleType("R"))))),
      caseClassDef("Transition", List("T", "N1", "N2"), List(parameter("transit", functionType(tuple2Type(simpleType("N1"), simpleType("T")), simpleType("N2")))))
    )

    /**
      * LALR オートマトンの各ノードを表現するデータ型の定義を出力する関数
      */
    lazy val nodeClassDefinitions: List[MemberDef] = automaton.nodes.toList.map { node =>
      if (automaton.start == node) caseObjectDef(nodeName(node))
      else automaton.state(node) match {
        case Inl(nt)            => caseClassDef(nodeName(node), List("NX"), List(parameter("prev", simpleType("NX")), parameter("value", nonTerminalType(nt))))
        case Inr(Inl(Inl(lit))) => caseClassDef(nodeName(node), List("NX"), List(parameter("prev", simpleType("NX")), parameter("value", literalType(lit))))
        case _                  => caseClassDef(nodeName(node), List("NX"), List(parameter("prev", simpleType("NX"))))
      }
    }

    /**
      * リテラルを表現するデータ型の定義を出力する関数
      * LALR オートマトンのノードで代用可能な時は出力しない
      */
    lazy val literalTokenTypeDefinitions: List[MemberDef] = automaton.syntax.literals.toList.collect {
      case literal if ! edgesFromStart.contains(Symbol(Terminal(literal))) =>
        caseClassDef(literalTokenTypeNames(literal), Nil, List(parameter("value", literalType(literal))))
    }

    /**
      * End of input を表現する値
      */
    lazy val eoiObjectDefinition: MemberDef = caseObjectDef("EoI")

    /**
      * キーワード(オペレータ)を表現するオブジェクトの定義を出力する関数
      * LALR オートマトンのノードで代用可能な時はそのノードオブジェクトを代入した定数の定義を出力する
      */
    lazy val keywordObjectDefinitions: List[MemberDef] = automaton.syntax.keywords.toList.map { k =>
      edgesFromStart.get(Symbol(Terminal(k))) match {
        case Some(node) =>
          lazyValDef(keywordTokenTypeNames(k),
                     parameterizedType(nodeName(node), List(objectType(startNode))),
                     callApply(objectRef(nodeName(node)), List(objectType(startNode)), List(objectRef(startNode))))
        case None =>
          caseObjectDef(keywordTokenTypeNames(k))
      }
    }

    /**
      * リテラルを表現するDSL関数の定義
      */
    lazy val literalDSLDefinitions: List[MemberDef] = automaton.syntax.literals.toList.map { literal =>
      functionDef(literalIdentifier(literal),
                  Nil,
                  List(parameter("value", literalType(literal))),
                  Nil,
                  literalTokenTypes(literal),
                  constructLiteralToken(literal, objectRef("value")))
    }

    /**
      * End of input を表すDSL関数の定義
      */
    lazy val eoiDSLDefinition: MemberDef = lazyValDef("$$", objectType("EoI"), objectRef("EoI"))

    /**
      * キーワード(オペレータ)を表すDSL関数の定義
      */
    lazy val keywordDSLDefinitions: List[MemberDef] = automaton.syntax.keywords.toList.map { k =>
      lazyValDef(k.kw, objectType(keywordTokenTypeNames(k)), objectRef(keywordTokenTypeNames(k)))
    }

    /**
      * transition の帰納的定義
      * - shift は transition
      * - reduce + transition は transition
      * - accept は transition
      *
      * EoI による transition は終了を表す
      */
    lazy val basicImplicits: List[MemberDef] = List (
      implicitFunctionDef(List("T", "N1", "N2"),
                          Nil,
                          List(parameter("shift", parameterizedType("Shift", simpleTypes("T", "N1", "N2")))),
                          parameterizedType("Transition", simpleTypes("T", "N1", "N2")),
                          callApply(objectRef("Transition"),
                                    simpleTypes("T", "N1", "N2"),
                                    List(lambda(List(parameter("state", simpleType("N1")), parameter("terminal", simpleType("T"))),
                                                methodCall(objectRef("shift"), "shift", Nil, List(objectRef("state"), objectRef("terminal"))))))),
      implicitFunctionDef(List("T", "N1", "N2", "N3"),
                          Nil,
                          List(parameter("reduce", parameterizedType("Reduce", simpleTypes("T", "N1", "N2"))),
                               parameter("transition", parameterizedType("Transition", simpleTypes("T", "N2", "N3")))),
                          parameterizedType("Transition", simpleTypes("T", "N1", "N3")),
                          callApply(objectRef("Transition"),
                                    simpleTypes("T", "N1", "N3"),
                                    List(lambda(List(parameter("state", simpleType("N1")), parameter("terminal", simpleType("T"))),
                                                methodCall(objectRef("transition"), "transit", Nil, List(methodCall(objectRef("reduce"), "reduce", Nil, List(objectRef("state"))), objectRef("terminal"))))))),
      implicitFunctionDef(List("NX", "R"),
                          Nil,
                          List(parameter("accept", parameterizedType("Accept", simpleTypes("NX", "R")))),
                          parameterizedType("Transition", List(objectType("EoI"), simpleType("NX"), simpleType("R"))),
                          callApply(objectRef("Transition"),
                                    List(objectType("EoI"), simpleType("NX"), simpleType("R")),
                                    List(lambda(List(parameter("state", simpleType("NX")), unusedParameter(objectType("EoI"))),
                                                methodCall(objectRef("accept"), "accept", Nil, List(objectRef("state"))))))),
      implicitFunctionDef(List("NX", "R"),
                          List(parameter("node", simpleType("NX"))),
                          List(parameter("transition", parameterizedType("Transition", List(objectType("EoI"), simpleType("NX"), simpleType("R"))))),
                          simpleType("R"),
                          methodCall(objectRef("transition"), "transit", Nil, List(objectRef("node"), objectRef("EoI"))))
    )

    /**
      * DSL のキーワードによる遷移を表現する関数の定義
      */
    lazy val keywordTransitionDefinitions: List[MemberDef] = automaton.syntax.keywords.toList.map { k =>
      implicitClassDef(List("N1", "N2"),
                       parameter("node", simpleType("N1")),
                       List(parameter("transition1", parameterizedType("Transition", List(objectType(keywordTokenTypeNames(k)), simpleType("N1"), simpleType("N2"))))),
                       functionDef(k.kw, Nil, Nil, Nil, simpleType("N2"), methodCall(objectRef("transition1"), "transit", Nil, List(objectRef("node"), objectRef(keywordTokenTypeNames(k))))) +:
                       automaton.syntax.terminals.toList.map { t =>
                         functionDef(k.kw,
                                     List("N3"),
                                     List(parameter("value", terminalType(t))),
                                     List(parameter("transition2", parameterizedType("Transition", List(terminalType(t), simpleType("N2"), simpleType("N3"))))),
                                     simpleType("N3"),
                                     methodCall(objectRef("transition2"), "transit", Nil, List(objectRef(k.kw), objectRef("value"))))
                       }
      )
    }

    /**
      * DSL のリテラルによる遷移を表現する関数の定義
      */
    lazy val literalTransitionDefinitions: List[MemberDef] = automaton.syntax.literals.toList.map { lit =>
      implicitClassDef(List("N1", "N2"),
                       parameter("node", simpleType("N1")),
                       List(parameter("transition", parameterizedType("Transition", List(literalTokenTypes(lit), simpleType("N1"), simpleType("N2"))))),
                       List(functionDef(literalIdentifier(lit),
                                        Nil,
                                        List(parameter("value", literalType(lit))),
                                        Nil,
                                        simpleType("N2"),
                                        methodCall(objectRef("transition"), "transit", Nil, List(objectRef("node"), constructLiteralToken(lit, objectRef("value")))))))
    }

    /**
      * Shift 操作を表す implicit value の定義
      */
    lazy val shiftImplicitDefinitions: List[MemberDef] = {
      automaton.shift.flatMap { case (from, map) =>
        val (typeParams, fromType) =
          if (automaton.start == from) (Nil, objectType(startNode))
          else (List("NX"), parameterizedType(nodeName(from), simpleTypes("NX")))

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
      implicitFunctionDef(List("NX"), Nil, Nil,
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
        if (automaton.start == via) (Nil, objectType(startNode))
        else (List("NX"), parameterizedType(nodeName(via), simpleTypes("NX")))

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
        if (automaton.start == path.head) (Nil, objectType(startNode))
        else (List("NX"), parameterizedType(nodeName(path.head), simpleTypes("NX")))

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
      edgesFromStart.get(Symbol(Terminal(literal))) match {
        case Some(node) => literal -> parameterizedType(nodeName(node), List(objectType(startNode)))
        case None       => literal -> simpleType(literalTokenTypeNames(literal))
      }
    }.toMap


    /**
      * リテラルを表現するデータを構築する
      * @param literal リテラルの種類
      * @param arg リテラルの値を計算する式
      * @return 構築したリテラルを表現するデータ
      */
    private def constructLiteralToken (literal: LiteralToken, arg: Expr): Expr = edgesFromStart.get(Symbol(Terminal(literal))) match {
      case Some(node) => callApply(objectRef(nodeName(node)), List(objectType(startNode)), List(objectRef(startNode), arg))
      case None       => callApply(objectRef(literalTokenTypeNames(literal)), Nil, List(arg))
    }

    private lazy val literalTokenTypeNames: Map[LiteralToken, String] = automaton.syntax.literals.map(_ -> generateUniqueName).toMap
    private lazy val keywordTokenTypeNames: Map[Keyword, String] = automaton.syntax.keywords.map(_ -> generateUniqueName).toMap
    private lazy val nodeName: Map[LRClosure, String] = automaton.nodes.map(_ -> generateUniqueName).toMap

    private lazy val startNode: String = nodeName(automaton.start)
    private lazy val edgesFromStart = automaton.edges(automaton.start)
  }
}
