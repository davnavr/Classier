module Classier.NET.Compiler.Parsing.Grammar

open System
open FParsec
open Classier.NET.Compiler
open Classier.NET.Compiler.ParserState

[<Flags>]
type NumType =
    | Decimal = 0uy
    | Double = 1uy
    | Float = 2uy
    | Signed = 0uy
    | Unsigned = 4uy
    | Integer = 8uy
    | Long = 16uy

type NumLiteral =
    { Base: byte
      FracPart: char list
      IntPart: char list
      Type: NumType }

type Expression =
    | AnonFunc of Function
    | CtorCall of
        {| Arguments: Expression list
           Type: TypeName |}
    | FuncCall of
        {| Arguments: Expression list list
           Target: Expression |}
    | IdentifierRef of Identifier
    | IfExpr of If
    | MatchExpr of Match
    | MemberAccess of Expression * Identifier
    | Nested of Expression
    | NumLit of NumLiteral
    | StrLit of string
    | ThrowExpr of Expression
    | TryExpr of Try
    | TupleLit of Expression list
    | UnitLit
    | VarAssignment of Assignment
and Assignment =
    { Target: Expression
      Value: Expression }
and If =
    { Condition: Expression
      Choice1: Statement list
      Choice2: Statement list }
and Match =
    { Against: Expression
      Cases: MatchCase list }
and MatchCase =
    { Body: Statement list
      Patterns: Pattern list }
and Pattern =
    | Constant of Expression
    | Default
    | TuplePattern of Param list
    | VarPattern of string * TypeName
and Statement =
    | Empty
    | IfStatement of If
    /// An expression whose result is evaluated then discarded.
    | IgnoredExpr of Expression
    /// Used in the body of classes and modules.
    | LocalMember of MemberDef
    | LocalVar of Variable
    | MatchStatement of Match
    | Return of Expression
    | Throw of Expression option
    | TryStatement of Try
    | While of Expression * Statement list
and Try =
    { TryBody: Statement list
      Handlers: MatchCase list
      Finally: Statement list }
and Variable =
    { Pattern: Pattern
      VarFlags: Flags
      Value: Expression option }
and Param =
    { Name: string
      Type: TypeName }
and GenericVariance =
    | NoVariance
    | Covariant
    | Contravariant
and GenericParam =
    { Name: string
      RequiredSuperClass: Identifier list
      RequiredInterfaces: TypeName list
      Variance: GenericVariance }
and Function =
    { Body: Statement list
      FuncDef: Definition
      GenericParams: GenericParam list
      Parameters: Param list list
      ReturnType: TypeName }
and ClassHeader =
    { PrimaryCtor: Constructor option
      SuperClass: Identifier list }
and TypeHeader =
    | Class of ClassHeader
    | DUnion
    | Interface
    | Module
    | Record
and TypeDef =
    { Definition: Definition
      GenericParams: GenericParam list
      Header: TypeHeader
      InitBody: Statement list
      Interfaces: TypeName list
      Members: MemberDef list }
and Constructor =
    { BaseCall: ConstructorBase
      Body: Statement list
      CtorDef: Definition
      Parameters: Param list }
and ConstructorBase =
    | SelfCall of Expression list
    | SuperCall of Expression list
and MemberDef =
    | Ctor of Constructor
    | Field of Variable
    | Function of Function
    | Property of 
        {| Get: Function option
           PropDef: Definition
           Set: Function option
           Value: Expression option |}
    | NestedType of TypeDef
    | DUnionCase of string * TypeName option

type CompilationUnit =
    { TypeDefs: TypeDef list
      Namespace: string list
      Usings: Identifier list list }

let parser: Parser<CompilationUnit, ParserState> =
    let colon = skipChar ':'
    let comma = skipChar ','
    let dquotes = skipChar '\"'
    let gtsign = skipChar '>'
    let lcurlybracket = skipChar '{' <?> "opening bracket"
    let lparen = skipChar '(' <?> "opening parenthesis"
    let ltsign = skipChar '<'
    let opequal = skipChar '='
    let period = skipChar '.' <?> "period"
    let rcurlybracket = skipChar '}' <?> "closing bracket"
    let rparen = skipChar ')' <?> "closing parenthesis"
    let semicolon = skipChar ';' <?> "semicolon"
    let underscore = skipChar '_' <?> "underscore"
    let lambdaOperator = skipString "=>" |> attempt <?> "lambda operator"

    let optList p = opt p |>> Option.defaultValue []

    let classDef, classDefRef = createParserForwardedToRef<TypeDef,_>()
    let identifier, identifierRef = createParserForwardedToRef<Identifier,_>()
    let ifStatement, ifStatementRef = createParserForwardedToRef<If,_>()
    let matchStatement, matchStatementRef = createParserForwardedToRef<Match,_>()
    let tryBlock, tryBlockRef = createParserForwardedToRef<Try,_>()
    let tupleExpr, tupleExprRef = createParserForwardedToRef<Expression list,_>()
    let typeName, typeNameRef = createParserForwardedToRef<TypeName,_>()
    let statement, statementRef = createParserForwardedToRef<Statement,_>()

    let ignored =
        choice
            [
                anyOf [ ' '; '\t' ]
                |> skipMany1
                <?> "whitespace";

                skipNewline;

                pstring "//"
                |>> ignore
                .>> restOfLine true
                <?> "single-line comment";

                skipString "/*"
                >>. skipCharsTillString "*/" true Int32.MaxValue
                <?> "multi-line comment";
            ]
        |> attempt
        |> skipMany
    let ignored1 = notEmpty ignored
    let ignored1OrSep =
        choice
            [
                ignored1
                followedBy lparen
            ]
    
    let defaultAccess = updateUserState (setFlags Flags.Public)
    let accessModifier lowest =
        let modifiers =
            [
                "public", Flags.Public
                "internal", Flags.Internal
                "protected", Flags.Protected
                "private", Flags.Private
            ]
            |> Seq.filter (fun (_, vis) -> vis <= lowest)
            |> Seq.map (fun (str, vis) ->
                skipString str
                |> attempt
                .>> updateUserState (setFlags vis))

        choice modifiers <?> "access modifier"
    let accessModifierOpt lowest =
        choice
            [
                accessModifier lowest
                defaultAccess
            ]

    let typeAnnotation =
        ignored
        >>. colon
        |> attempt
        >>. ignored
        >>. typeName
        <?> "type annotation"
    let typeAnnotationOpt =
        typeAnnotation
        |> opt
        |>> Option.defaultValue Inferred

    let modifier name flag =
        skipString name
        >>. ignored1
        |> attempt
        >>. updateUserState (setFlags flag)
        |> optional
    
    let separator p = ignored >>. attempt p .>> ignored

    let block p =
        lcurlybracket
        |> attempt
        >>. ignored
        >>. p
        .>> ignored
        .>> rcurlybracket
    let blockChoice p =
        choice p
        .>> ignored
        |> many
        |> block
    let statementBlock = blockChoice [ statement ]

    let genericArgs =
        ltsign
        >>. sepBy1 typeName (separator comma)
        |> attempt
        .>> gtsign
        |> optList
        <?> "generic arguments"

    let identifierStr =
        asciiLetter
        .>>. manyChars (asciiLetter <|> digit)
        |>> String.Concat
        <?> "identifier"
    let identifierList =
        sepBy1 identifier (separator period |> attempt)
    let identifierFull =
        identifierList
        |>> Identifier
        <?> "fully qualified identifier"

    let paramTuple =
        lparen
        .>> ignored
        >>. sepBy
                (identifierStr
                .>>. typeAnnotationOpt
                |>> fun (name, ptype) ->
                    { Name = name
                      Type = ptype })
                (separator comma)
        .>> rparen
        <?> "parameters"
    let paramTupleList =
        paramTuple
        .>> ignored
        |> many1
        <?> "parameter list"

    let expression =
        let decimalChars = [ '0'..'9' ]

        let expr = OperatorPrecedenceParser<_,_,_>()
        let exprParser = expr.ExpressionParser <?> "expression"

        let functionCall targetExpr args name =
            let target = MemberAccess (targetExpr, { Name = name; GenericArgs = List.empty })
            FuncCall {| Arguments = args; Target = target |}
        let assignment t target value = t { Target = target; Value = value }

        let strChar = manySatisfy (fun c -> c <> '\\' && c <> '"')
        let strEscaped = skipString "\\u" >>. parray 4 hex |>> String

        [
            "equals", "==", 38, Associativity.Left

            // Mathematical operators
            "add", "+", 40, Associativity.Left
            "subtract", "-", 40, Associativity.Left
            "multiply", "*", 50, Associativity.Left
            "divide", "/", 50, Associativity.Left

            // Boolean operators
            "or", "||", 34, Associativity.Left
            "and", "&&", 36, Associativity.Left
        ]
        |> Seq.map (fun (name, op, prec, assoc) ->
            let map expr1 expr2 =
                functionCall expr1 [ [ expr2 ] ] name
            InfixOperator<_,_,_>(op, ignored, prec, assoc, map))
        |> Seq.cast<Operator<_,_,_>>
        |> Seq.append
            [
                InfixOperator<_,_,_>(">>", ignored, 18, Associativity.Left, fun f g -> invalidOp "Composition operator is not implemented");
                InfixOperator<_,_,_>("|>", ignored, 20, Associativity.Left, fun args f -> FuncCall {| Arguments = [ [ args ] ]; Target = f |});
                PrefixOperator<_,_,_>("!", ignored, 32, true, fun exp -> functionCall exp [] "not");
                PrefixOperator<_,_,_>("-", ignored, 60, true, fun exp -> functionCall exp [] "negate");
                InfixOperator<_,_,_>("<-", ignored, 100, Associativity.Right, assignment VarAssignment);
            ]
        |> Seq.iter expr.AddOperator

        expr.TermParser <-
            choice
                [
                    between
                        dquotes
                        dquotes
                        (stringsSepBy strChar strEscaped)
                    |>> StrLit
                    <?> "string literal"

                    tupleExpr
                    <?> "tuple"
                    |>> (fun ex ->
                        match ex with
                        | [] -> UnitLit
                        | [ nested ] -> nested
                        | _ -> TupleLit ex);

                    ifStatement |>> IfExpr <?> "if expression"

                    matchStatement |>> MatchExpr <?> "match expression"

                    skipString "throw"
                    >>. ignored1
                    |> attempt
                    >>. exprParser
                    |>> ThrowExpr

                    skipString "new"
                    >>. ignored1
                    |> attempt
                    >>. opt identifierFull
                    .>> ignored
                    .>>. (tupleExpr <?> "constructor arguments")
                    <?> "constructor call"
                    |>> (fun (ctype, args) ->
                        CtorCall
                            {| Arguments = args
                               Type = Option.defaultValue Inferred ctype |});

                    tryBlock |>> TryExpr <?> "try expression"

                    identifier |>> IdentifierRef;

                    pchar '0'
                    >>. choice
                        [
                            anyOf [ 'b'; 'B' ] >>. preturn [ '0'; '1'; ];
                            anyOf [ 'x'; 'X' ] >>. preturn (decimalChars @ [ 'a'..'z' ] @ [ 'A'..'Z' ]);
                        ]
                    |> attempt
                    <|> preturn decimalChars
                    <|> (pchar '.' >>. preturn [])
                    >>= (fun chars ->
                        let digits c =
                            attempt (anyOf c) .>> (skipMany (pchar '_') <?> "digit separator") |> many1
                        let decimalDigits = digits decimalChars

                        (match chars.Length with
                            | 0 -> decimalDigits |>> fun digits -> [], digits
                            | 10 ->
                                decimalDigits
                                .>>. opt (pchar '.' |> attempt >>. decimalDigits)
                                |>> fun (idigits, fdigits) ->
                                    match fdigits with
                                    | Some _ -> idigits, fdigits.Value
                                    | None -> idigits, []
                            | _ -> digits chars |>> fun digits -> digits, [])
                        >>= fun (idigits, fdigits) ->
                            let suffixes pairs none =
                                pairs
                                |> Seq.map (fun (s, f) -> pstringCI s >>. preturn f)
                                |> choice
                                |> opt
                                |>> Option.defaultValue none
                                <?> "numeric suffix"

                            if List.isEmpty fdigits then
                                suffixes
                                    [
                                        "u", NumType.Integer ||| NumType.Unsigned
                                        "l", NumType.Long
                                        "ul", NumType.Long ||| NumType.Unsigned
                                        "lu", NumType.Long ||| NumType.Unsigned
                                    ]
                                    NumType.Integer
                                |>> fun intType ->
                                    NumLit
                                        { Base = byte(chars.Length)
                                          FracPart = []
                                          IntPart = idigits
                                          Type = intType }
                            else
                                suffixes
                                    [
                                        "d", NumType.Double
                                        "f", NumType.Float
                                        "m", NumType.Decimal
                                    ]
                                    NumType.Double
                                |>> fun floatType ->
                                    NumLit
                                        { Base = byte(10)
                                          FracPart = fdigits
                                          IntPart = idigits
                                          Type = floatType })
                    <?> "numeric literal";
                ]
            .>> ignored
            >>= fun target ->
                let memberAccess =
                    period
                    |> attempt
                    >>. ignored
                    >>. identifier
                    .>> ignored
                    <?> "member access"
                    |> many1
                    |>> fun members ->
                        fun t ->
                            Seq.fold
                                (fun prev tmember -> MemberAccess (prev, tmember))
                                t
                                members
                let funcCall =
                    tupleExpr
                    .>> ignored
                    |> many1 <?>
                    "argument list"
                    |>> fun args ->
                        fun t ->
                            match args with
                            | [] -> target
                            | _ -> FuncCall {| Arguments = args; Target = t |}
                memberAccess <|> funcCall
                .>> ignored
                |> many1
                <|> preturn []
                |>> Seq.fold
                    (fun prev next -> next prev)
                    target
        exprParser
    let expressionInParens = // NOTE: Tuples might not parse correctly here.
        lparen
        |> attempt
        >>. ignored
        >>. expression
        .>> rparen
    let equalsExpression =
        opequal
        |> attempt
        >>. ignored
        >>. expression

    let lambdaBody =
        lambdaOperator
        |> attempt
        >>. ignored
        >>. expression
        |>> Return
        |>> List.singleton
        .>> ignored
        .>> semicolon

    let pattern =
        [
            identifierStr
            .>> ignored
            .>>. typeAnnotationOpt
            |>> VarPattern
            <?> "variable pattern"

            paramTuple
            |>> TuplePattern
            <?> "tuple deconstruction"
        ]
        |> choice

    let matchCases =
        let matchPattern =
            [
                pattern

                expression
                |>> Constant
                <?> "constant pattern"

                underscore
                >>. ignored
                |> attempt
                >>. preturn Default
                <?> "default pattern"
            ]
            |> choice
        sepBy1 matchPattern (separator comma |> attempt)
        .>> ignored
        .>> lambdaOperator
        |> attempt
        .>> ignored
        .>>. expression
        .>> ignored
        .>> semicolon
        |>> (fun (patterns, expr) -> { Body = [ Return expr ]; Patterns = patterns })
        .>> ignored
        |> many1
        |> block
        <?> "cases"

    tupleExprRef :=
        lparen
        |> attempt
        >>. ignored
        >>. sepBy expression (separator comma)
        .>> rparen

    typeNameRef :=
        choiceL
            [
                sepBy1
                    identifierFull
                    (pchar '|' |> separator |> attempt)
                |>> fun tlist ->
                    match tlist with
                    | [ one ] -> one
                    | _ -> Union tlist

                skipChar '_'
                >>. preturn Inferred;

                between
                    lparen
                    rparen
                    (sepBy typeName (separator comma))
                |>> Tuple
                <?> "tuple";
            ]
            "type name"

    identifierRef :=
        identifierStr
        .>> ignored
        .>>. genericArgs
        |>> fun (name, gparams) ->
            { Name = name
              GenericArgs = gparams }

    ifStatementRef :=
        skipString "if"
        >>. ignored
        >>. expressionInParens
        .>> ignored
        .>>. statementBlock
        |> attempt
        .>>. choice
            [
                ignored
                >>. skipString "else"
                |> attempt
                >>. choice
                    [
                        ignored1
                        >>. ifStatement
                        |> attempt
                        |>> fun e -> [ IfStatement e ]

                        ignored
                        >>. statementBlock
                        |> attempt;
                    ]
                <?> "else or else-if"

                preturn []
            ]
        |>> fun ((condition, body), rest) ->
            { Condition = condition
              Choice1 = body
              Choice2 = rest }

    matchStatementRef :=
        skipString "match"
        >>. ignored
        >>. expressionInParens
        .>> ignored
        .>>. matchCases
        |>> fun (against, cases) -> { Against = against; Cases = cases }

    statementRef :=
        choiceL
            [
                semicolon
                >>. preturn Empty
                <?> "empty statement";
                
                skipString "var"
                >>. ignored1OrSep
                |> attempt
                >>. updateUserState newFlags
                >>. updateUserState (setFlags Flags.Mutable)
                >>. pattern
                .>> ignored
                .>>. opt (equalsExpression .>> ignored .>> semicolon)
                .>>. getUserState
                .>> updateUserState popFlags
                |>> (fun ((p, value), state) ->
                    LocalVar
                        { Pattern = p
                          VarFlags = ParserState.currentFlags state
                          Value = value })
                <?> "mutable variable"

                skipString "let"
                >>. ignored1OrSep
                |> attempt
                >>. updateUserState newFlags
                >>. pattern
                .>> ignored
                .>>. getUserState
                >>= (fun (p, state) ->
                    let value =
                        equalsExpression
                        |> attempt
                        .>> ignored
                        .>> semicolon
                    let rest =
                        (match p with
                        | VarPattern (name, vtype) ->
                            match vtype with
                            | Inferred ->
                                [
                                    value

                                    paramTupleList
                                    .>> ignored
                                    .>>. typeAnnotationOpt
                                    .>> ignored
                                    .>>. (statementBlock <?> "local function body")
                                    |>> fun ((parameters, retType), body) ->
                                        { Body = body 
                                          FuncDef =
                                            { Name = name
                                              Flags = ParserState.currentFlags state }
                                          GenericParams = []
                                          Parameters = parameters
                                          ReturnType = retType }
                                        |> AnonFunc
                                ]
                                |> choice
                                |> Some
                            | _ -> None
                        | _ -> None)
                    rest
                    |> Option.defaultValue value
                    |>> fun value ->
                        { Pattern = p
                          VarFlags = ParserState.currentFlags state
                          Value = Some value })
                .>> updateUserState popFlags
                <?> "local variable or function"
                |>> LocalVar

                skipString "while"
                >>. ignored
                |> attempt
                >>. lparen
                >>. expression
                .>> rparen
                .>> ignored
                .>>. statementBlock
                |>> While
                <?> "while loop"

                skipString "return"
                >>. ignored1
                |> attempt
                >>. expression
                |>> Return
                <?> "return statement"
                .>> ignored
                .>> semicolon

                skipString "throw"
                >>. choice
                    [
                        ignored1
                        |> attempt
                        >>. expression
                        |>> Some

                        ignored
                        >>. semicolon
                        >>. preturn None
                    ]
                |> attempt
                |>> Throw
                <?> "throw statement"

                ifStatement |>> IfStatement <?> "if statement"

                matchStatement |>> MatchStatement <?> "match statement"

                tryBlock |>> TryStatement <?> "try statement"
                
                expression
                .>> ignored
                .>>. choice
                    [
                        followedBy rcurlybracket
                        >>. preturn Return
                        <?> "implicit return"

                        semicolon
                        >>. preturn IgnoredExpr
                        <?> "ignored expression"
                    ]
                |>> fun (expr, statement) -> statement expr
            ]
            "statement"

    tryBlockRef :=
        skipString "try"
        >>. ignored
        >>. statementBlock
        |> attempt
        .>>. (ignored
            >>. skipString "catch"
            >>. ignored
            |> attempt
            >>. matchCases
            |> optList)
        .>>.
            (ignored
            >>. skipString "finally"
            >>. ignored
            |> attempt
            >>. statementBlock
            <?> "finally block"
            |> optList)
        >>= fun ((tryBlock, catchBlock), finallyBlock) ->
            if List.isEmpty catchBlock && List.isEmpty finallyBlock then
                fail "Expected at least one catch or finally block."
            else
                preturn
                    { TryBody = tryBlock
                      Handlers = catchBlock
                      Finally = finallyBlock }

    let extends =
        skipString "extends"
        .>> ignored1
        |> attempt
        >>. identifierList
        |> optList

    let implements =
        skipString "implements"
        .>> ignored1
        |> attempt
        >>. sepBy1 identifierFull (separator comma)
        |> optList
        <?> "interface implementations"

    let genericParams =
        let genericParam =
            choice
                [
                    skipString "in"
                    >>. ignored1
                    |> attempt
                    >>. preturn Contravariant;

                    skipString "out"
                    >>. ignored1
                    |> attempt
                    >>. preturn Covariant;

                    preturn NoVariance;
                ]
            .>>. identifierStr
            |> attempt
            .>> ignored
            .>>. extends
            .>>. implements
            <?> "generic parameter"
            |>> fun (((variance, name), super), iimpl) ->
                { Name = name
                  RequiredInterfaces = iimpl
                  RequiredSuperClass = super
                  Variance = variance }
        ltsign
        |> attempt
        >>. sepBy1 genericParam (separator comma)
        .>> gtsign
        |> optList
        <?> "generic parameters"

    let functionBody =
        choiceL
            [
                statementBlock
                lambdaBody
                semicolon >>. preturn []
            ]
            "function body"

    let functionDef =
        modifier "inline" Flags.Inline
        >>. getUserState
        .>>. identifierStr
        .>> ignored
        .>>. genericParams
        .>> ignored
        .>>. paramTupleList
        |> attempt
        .>>. typeAnnotationOpt
        .>> ignored
        .>>. functionBody
        |>> fun (((((state, name), gparams), fparams), retType), body) ->
            Function
                { Body = body
                  FuncDef = Definition.ofState name state
                  GenericParams = gparams
                  Parameters = fparams
                  ReturnType = retType }

    let methodDef =
        modifier "abstract" Flags.Abstract
        >>. functionDef
        <?> "method definition"

    let typeDef word =
        skipString word
        >>. ignored1
        |> attempt
        >>. getUserState

    let unionDef = // TODO: Replace dunion with case classes like in Scala or do what Kotlin does https://www.howtobuildsoftware.com/index.php/how-do/SmZ/kotlin-discriminated-union-kotlin-and-discriminated-unions-sum-types
        typeDef "union" // TODO: Pick better keyword to use than "union".
        .>>. identifierStr
        .>>. genericParams
        .>> ignored
        .>>. blockChoice
            [
                identifierStr
                .>>. typeAnnotationOpt
                .>> ignored
                .>> comma
                <?> "union case"
                |>> fun (name, ctype) ->
                    match ctype with
                    | Inferred -> DUnionCase (name, None)
                    | _ -> DUnionCase (name, Some ctype)
            ]
        <?> "discriminated union"
        |>> fun (((state, name), gparams), cases) ->
            { Definition = Definition.ofState name state
              GenericParams = gparams
              Header = DUnion
              InitBody = []
              Interfaces = []
              Members = cases }

    let interfaceDef =
        typeDef "interface"
        .>>. identifierStr
        .>>. genericParams
        .>>. implements
        .>> ignored
        .>>. blockChoice
            [
                accessModifier Flags.Internal
                >>. ignored1
                >>. choice
                    [
                        methodDef
                    ]
            ]
        |>> (fun ((((state, name), gparams), iimpls), members) ->
            { Definition = Definition.ofState name state
              GenericParams = gparams
              Header = Interface
              InitBody = []
              Interfaces = iimpls
              Members = members })
        <?> "interface definition"

    /// Allows both members and statements.
    let memberBlockInit members =
        blockChoice
            [
                accessModifier Flags.Private
                >>. ignored1
                |> attempt
                >>. choice members
                |>> LocalMember

                statement
            ]
        |>> fun st ->
            let members = // TODO: Optimize this?
                st
                |> List.choose (function
                    | LocalMember m -> Some m
                    | _ -> None)
            let body =
                st
                |> List.choose (fun s ->
                    match s with
                    | LocalMember _ -> None
                    | _ -> Some s)
            members, body

    let classNested = classDef <?> "nested class" |>> NestedType
    let classPrimaryCtor =
        accessModifierOpt Flags.Private
        >>. ignored
        >>. opt (paramTuple .>> ignored)
    let classExtends =
        extends
        .>>. optList (tupleExpr .>> ignored)
        .>> ignored

    // TODO: Add "data class" which are records
    classDefRef :=
        modifier "abstract" Flags.Abstract
        >>. modifier "inheritable" Flags.Inheritable
        >>. modifier "mutable" Flags.Mutable
        >>. typeDef "class"
        |> attempt
        .>>. identifierStr
        .>>. genericParams
        .>> ignored
        .>>. classPrimaryCtor
        .>>. classExtends
        .>>. implements
        .>> ignored
        .>>. memberBlockInit
            [
                methodDef
                classNested
            ]
        <?> "class definition"
        |>> fun ((((((state, name), gparams), ctorParams), (superclass, baseArgs)), iimpls), (members, body)) ->
            { Definition = Definition.ofState name state
              GenericParams = gparams
              Header = Class
                { PrimaryCtor = None
                  SuperClass = superclass }
              InitBody = body
              Interfaces = iimpls
              Members = members }

    let moduleDef =
        typeDef "module"
        .>>. identifierStr
        .>>. genericParams
        .>> ignored
        .>>. memberBlockInit
            [
                unionDef |>> NestedType
                interfaceDef |>> NestedType
                classNested
                functionDef <?> "function definition"
            ]
        <?> "module definition"
        |>> fun (((state, name), gparams), (members, body)) ->
            { Definition = Definition.ofState name state
              GenericParams = gparams
              Header = Module
              InitBody = body
              Interfaces = []
              Members = members }

    ignored
    >>. optList
        (skipString "namespace"
        >>. ignored1
        |> attempt
        >>. sepBy1 identifierStr (separator period)
        >>= (fun names ->
            updateSymbols (SymbolTable.addNamespace names)
            >> pushParent (Identifier.ofStrings names)
            |> updateUserState
            >>. preturn names)
        .>> ignored
        .>> semicolon
        <?> "namespace declaration")
    .>> ignored
    .>>. many
        (skipString "use"
        >>. ignored1
        |> attempt
        >>. identifierList
        .>> ignored
        .>> semicolon
        <?> "use statement"
        .>> ignored)
    .>>. (accessModifier Flags.Internal
         >>. ignored1
         >>. choice
             [
                 classDef
                 interfaceDef
                 moduleDef
                 unionDef
             ]
         .>> ignored
         |> many1)
    .>> eof
    .>> updateUserState (clearAllFlags >> clearAllParents)
    |>> fun ((ns, uses), defs) ->
        { TypeDefs = defs
          Namespace = ns
          Usings = uses }
