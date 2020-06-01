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
      FuncDef: Definition option
      GenericParams: GenericParam list
      Parameters: Param list list
      ReturnType: TypeName }
and ClassHeader =
    { PrimaryCtor: Constructor
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
      CtorFlags: Flags
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

    let identifier, identifierRef = createParserForwardedToRef<Identifier,_>()
    let ifStatement, ifStatementRef = createParserForwardedToRef<If,_>()
    let matchStatement, matchStatementRef = createParserForwardedToRef<Match,_>()
    let nestedTypes, nestedTypesRef = createParserForwardedToRef<MemberDef,_>()
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
    let modifiers pairs =
        pairs
        |> Seq.map (fun (name, flag) -> modifier name flag)
        |> Seq.reduce (fun one two -> one >>. two)
    
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
        // TODO: Check that the correct character is used when parsing escape sequences, and allow stuff like \n or \r.
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

                    paramTuple
                    .>> ignored
                    .>> ignored
                    .>> lambdaOperator
                    |> attempt
                    .>> ignored
                    .>>. exprParser
                    <?> "anonymous function"
                    |>> fun (parameters, retVal) ->
                        { Body = [ Return retVal ]
                          FuncDef = None
                          GenericParams = []
                          Parameters = [ parameters ]
                          ReturnType = Inferred }
                        |> AnonFunc

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
        .>>. choice
            [
                expression |>> (Return >> List.singleton)
                statementBlock
            ]
        .>> ignored
        .>> semicolon
        |>> (fun (patterns, body) -> { Body = body; Patterns = patterns })
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
                <?> "tuple"
                |>> function
                | [] -> Primitive PrimitiveType.Unit
                | items -> Tuple items
            ]
            "type name"
        .>>. opt
            (ignored
            >>. lambdaOperator
            >>. ignored
            >>. typeName
            |> attempt)
        |>> fun (tname, funcRet) ->
            match funcRet with
            | Some _ -> FuncType {| ParamType = tname; ReturnType = funcRet.Value |}
            | None -> tname

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
                          VarFlags = currentFlags state
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
                                              Flags = currentFlags state }
                                            |> Some
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
                          VarFlags = currentFlags state
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
                  FuncDef = Some (Definition.ofState name state)
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

    let interfaceDef =
        typeDef "interface"
        .>>. identifierStr
        .>>. genericParams
        .>>. implements
        .>> ignored
        .>>. blockChoice
            [
                updateUserState newFlags
                >>. accessModifier Flags.Internal
                >>. ignored1
                |> attempt
                >>. choice
                    [
                        methodDef
                        nestedTypes
                    ]
                .>> updateUserState popFlags
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
                updateUserState newFlags
                >>. accessModifier Flags.Private
                >>. ignored1
                |> attempt
                >>. choice members
                .>> updateUserState popFlags
                |>> LocalMember

                statement
            ]
        |>> fun st ->
            let members =
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

    let classDef =
        let primaryCtor =
            updateUserState newFlags
            >>. accessModifierOpt Flags.Private
            >>. ignored
            >>. getUserState
            .>> updateUserState popFlags
            .>>. optList (paramTuple .>> ignored)
        let classExtends =
            extends
            .>>. optList (tupleExpr .>> ignored)
            .>> ignored

        modifiers
            [
                "abstract", Flags.Abstract
                "inheritable", Flags.Inheritable
                "mutable", Flags.Mutable
            ]
        >>. choice
            [
                skipString "data"
                >>. ignored1
                |> attempt
                >>. preturn true

                preturn false
            ]
        .>>. typeDef "class"
        |> attempt
        .>>. identifierStr
        .>>. genericParams
        .>> ignored
        .>>. primaryCtor
        .>>. classExtends
        .>>. implements
        .>> ignored
        .>>. choice
            [
                [
                    methodDef
                    nestedTypes
                ]
                |> memberBlockInit

                semicolon >>. preturn ([], [])
            ]
        <?> "class definition"
        |>> fun (((((((isRecord, state), name), gparams), (ctorState, ctorParams)), (superclass, baseArgs)), iimpls), (members, body)) ->
            let primaryCtor =
                let ctor flags =
                    { BaseCall = SuperCall baseArgs
                      Body = body
                      CtorFlags = flags
                      Parameters = ctorParams }
                match ctorParams with
                | [] -> ctor Flags.Public
                | _ -> ctor (currentFlags ctorState)
            let recordMembers () =
                ctorParams
                |> List.map (fun p ->
                    { Body = [ { Name = p.Name; GenericArgs = [] } |> IdentifierRef |> Return ]
                      FuncDef =
                        { Flags = Flags.Public
                          Name = p.Name }
                        |> Some
                      GenericParams = []
                      Parameters = []
                      ReturnType = p.Type }
                    |> Function)
                |> List.append
                    [
                        { Body = []
                          FuncDef =
                            { Flags = Flags.Public ||| Flags.Override
                              Name = "equals" }
                            |> Some
                          GenericParams = []
                          Parameters = [ [ { Name = "obj"; Type = Inferred } ] ]
                          ReturnType = Primitive PrimitiveType.Boolean }
                        |> Function
                    ]

            { Definition = Definition.ofState name state
              GenericParams = gparams
              Header = Class
                { PrimaryCtor = primaryCtor
                  SuperClass = superclass }
              InitBody = body
              Interfaces = iimpls
              Members =
                if isRecord
                then members @ recordMembers()
                else members }

    let moduleDef =
        typeDef "module"
        .>>. identifierStr
        .>>. genericParams
        .>> ignored
        .>>. memberBlockInit
            [
                functionDef <?> "function definition"
                nestedTypes
            ]
        <?> "module definition"
        |>> fun (((state, name), gparams), (members, body)) ->
            { Definition = Definition.ofState name state
              GenericParams = gparams
              Header = Module
              InitBody = body
              Interfaces = []
              Members = members }

    nestedTypesRef :=
        [
            classDef <?> "nested class"
            interfaceDef <?> "nested interface"
            moduleDef <?> "nested module"
        ]
        |> choice
        |>> NestedType

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
    .>>. (updateUserState newFlags
         >>. accessModifier Flags.Internal
         >>. ignored1
         |> attempt
         >>. choice
             [
                 classDef
                 interfaceDef
                 moduleDef
             ]
         .>> ignored
         .>> updateUserState popFlags
         |> many1)
    .>> eof
    .>> updateUserState (clearAllFlags >> clearAllParents)
    |>> fun ((ns, uses), defs) ->
        { TypeDefs = defs
          Namespace = ns
          Usings = uses }
