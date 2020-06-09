module Classier.NET.Compiler.Parser

open System
open FParsec
open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.GlobalType
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.ParserState
open Classier.NET.Compiler.TypeSystem

let compilationUnit: Parser<CompilationUnit, ParserState> =
    let colon = skipChar ':'
    let comma = skipChar ','
    let dquotes = skipChar '\"' <?> "quotation mark"
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

    let position: Parser<Position, _> = fun stream -> Reply(stream.Position)

    let optList p = opt p |>> Option.defaultValue []

    let validateFlags f msg =
        userStateSatisfies (ParserState.currentFlags >> f >> not) <?> msg

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

    let accessed lowestAccess p =
        let modifiers =
            [
                "public", Flags.Public
                "internal", Flags.Internal
                "protected", Flags.Protected
                "private", Flags.Private
            ]
            |> Seq.map (fun (str, vis) ->
                if vis <= lowestAccess then
                    skipString str
                    .>> ignored1OrSep
                    |> attempt
                    .>> updateUserState (setFlags vis)
                else
                    sprintf "An access modifier of %s is not valid here" (vis.ToString().ToLower()) |> fail)

        updateUserState newFlags
        >>. choice
            [
                choiceL modifiers "access modifier"
                |> attempt
                >>. p

                updateUserState (setFlags Flags.Public)
                >>. p
            ]
        .>> updateUserState popFlags

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
        |> attempt
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
        |> attempt
        .>> ignored
        >>. sepBy
                (identifierStr
                .>>. typeAnnotationOpt
                <?> "parameter"
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
            let target = MemberAccess (targetExpr, Identifier.ofString name)
            FuncCall {| Arguments = args; Target = target |}
        let assignment t target value = t { Target = target; Value = value }

        let strChar = manySatisfy (fun c -> c <> '\\' && c <> '"' && c <> '\n')
        let strEscaped =
            skipChar '\\'
            |> attempt
            >>. choice
                [
                    skipChar 'n' >>% '\n'
                    skipChar 'r' >>% '\r'
                    skipChar 't' >>% '\t'

                    skipChar 'u'
                    |> attempt
                    >>. parray 4 hex
                    |>> (fun chars -> Convert.ToUInt32(String chars, 16) |> char)
                ]
            |>> string

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
                InfixOperator<_,_,_>(">>", ignored, 18, Associativity.Left, fun f g -> FuncComp (f, g));
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
                    
                    [
                        skipString "true" >>% true
                        skipString "false" >>% false
                    ]
                    |> choice
                    |>> BoolLit
                    <?> "boolean literal"

                    identifier |>> IdentifierRef;

                    pchar '0'
                    >>. choice
                        [
                            anyOf [ 'b'; 'B' ] >>% [ '0'; '1'; ];
                            anyOf [ 'x'; 'X' ] >>% (decimalChars @ [ 'a'..'z' ] @ [ 'A'..'Z' ]);
                        ]
                    |> attempt
                    <|>% decimalChars
                    <|> (pchar '.' >>% [])
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
                                |> Seq.map (fun (s, f) -> pstringCI s >>% f)
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
                <|>% []
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
                >>% Default
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

    let simpleTypeName =
        choiceL
            [
                PrimitiveType.names
                |> Seq.map (fun pair -> skipString (pair.Value) >>% pair.Key)
                |> choice
                |>> Primitive

                identifierFull

                skipChar '_' >>% Inferred

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

    typeNameRef :=
        sepBy1
            simpleTypeName
            (pchar '|' |> separator |> attempt)
        |>> (fun tlist ->
            match tlist with
            | [ one ] -> one
            | _ -> Union tlist)
        .>>. opt
            (ignored
            >>. lambdaOperator
            >>. ignored
            >>. typeName
            |> attempt)
        |>> fun (paramsType, retType) ->
            match retType with
            | Some _ -> FuncType {| ParamType = paramsType; ReturnType = retType.Value |}
            | None -> paramsType

    identifierRef :=
        identifierStr
        .>> ignored
        .>>. genericArgs
        |>> fun (name, gparams) ->
            { Name = name
              Generics = List.map GenericArg gparams }

    ifStatementRef :=
        skipString "if"
        >>. ignored1OrSep
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
                semicolon >>% Empty <?> "empty statement"
                
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
                >>. position
                .>>. pattern
                .>> ignored
                .>>. getUserState
                >>= (fun ((pos, p), state) ->
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
                        >>% None
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
                        >>% Return
                        <?> "implicit return"

                        semicolon
                        >>% IgnoredExpr
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
                    >>% Contravariant;

                    skipString "out"
                    >>. ignored1
                    |> attempt
                    >>% Covariant;

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
    let genericIdentifier: Parser<Identifier, _> =
        identifierStr
        .>> ignored
        .>>. genericParams
        |>> fun (name, gparams) ->
            { Name = name
              Generics = List.map GenericParam gparams }
    let genericDefinition =
        position
        .>>. genericIdentifier
        .>>. getUserState
        |>> (fun ((pos, id), state) -> Definition.ofIdentifier state pos id)

    let functionBody =
        choiceL
            [
                statementBlock
                lambdaBody
                semicolon >>% []
            ]
            "function body"

    let functionHeader f =
        genericDefinition
        .>> ignored
        .>>. paramTupleList
        |> attempt
        .>>. getUserState
        >>= fun ((def, fparams), state) ->
            let containsDup =
                FunctionDef.placeholder def fparams
                |> f
                |> (state.Members.Head).Contains

            if containsDup then
                Param.toString fparams
                |> sprintf "A member with the signature %s %s already exists in the same scope" (string def)
                |> fail
            else
                preturn (def, fparams)

    let functionDef f =
        modifiers
            [
                "inline", Flags.Inline
            ]
        >>. skipString "def"
        >>. ignored1
        >>. functionHeader f
        .>>. typeAnnotationOpt
        .>> ignored
        .>>. functionBody
        >>= fun (((def, fparams), retType), body) ->
            let func =
                { FuncDef = def
                  Function =
                    { Body = body
                      Parameters = fparams
                      ReturnType = retType }
                  SelfIdentifier = None }
                |> f

            addMember func
            |> updateUserState
            >>% func

    let methodDef =
        modifiers
            [
                //"inline", Flags.Inline
                "abstract", Flags.Abstract
                "sealed", Flags.Sealed
                "override", Flags.Override
                "virtual", Flags.Virtual
            ]
        >>. validateFlags
                (fun flags -> (flags.HasFlag Flags.Abstract) && (flags &&& Flags.MethodImplMask > Flags.None))
                "Invalid modifiers on abstract method"
        >>. validateFlags
                (fun flags -> (flags &&& Flags.MethodImplMask).HasFlag(Flags.Sealed ||| Flags.Virtual))
                "Virtual methods cannot be sealed"
        >>. functionDef Method
        <?> "method definition"

    let typeDef word =
        skipString word
        >>. ignored1
        |> attempt

    let interfaceDef =
        typeDef "interface"
        >>. genericDefinition
        .>>. implements
        .>> ignored
        .>> updateUserState newMembers
        .>> blockChoice
            [
                [
                    methodDef
                    nestedTypes
                ]
                |> (choice >> accessed Flags.Internal)
            ]
        .>>. getUserState
        .>> updateUserState popMembers
        <?> "interface definition"
        |>> fun ((def, iimpls), state) ->
            { TypeDef = def
              Header = Interface
              InitBody = []
              Interfaces = iimpls
              Members = state.Members.Head }

    /// Allows both members and statements.
    let memberBlockInit members =
        updateUserState newMembers
        >>. blockChoice
            [
                attempt statement

                choice members
                |> accessed Flags.Private
                |>> LocalMember
            ]
        |> attempt
        .>>. getUserState
        .>> updateUserState popMembers
        |>> fun (st, state) ->
            let body =
                st
                |> List.choose (fun s ->
                    match s with
                    | LocalMember _ -> None
                    | _ -> Some s)
            state.Members.Head, body

    let classDef =
        let classCtor =
            optList (paramTuple .>> ignored)
            .>>. getUserState
            |> accessed Flags.Private
            |>> fun (ctorParams, state) ->
                let flags =
                    match ctorParams with
                    | [] -> Flags.Public
                    | _ -> currentFlags state
                fun body baseArgs ->
                    { BaseCall = SuperCall baseArgs
                      Body = body
                      CtorFlags = flags
                      Parameters = ctorParams }
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
                // Append the record members
                >>% (fun (parameters: Param list) pos ->
                    parameters
                    |> List.map (fun param ->
                        { FunctionDef.generatedDef with
                            FuncDef =
                              { Flags = Flags.Public
                                Identifier = Identifier.ofString param.Name
                                Position = pos }
                            Function =
                              { Body =
                                  [
                                      Identifier.ofString param.Name
                                      |> IdentifierRef
                                      |> Return
                                  ]
                                Parameters = []
                                ReturnType = param.Type } }
                        |> Method)
                    |> List.append
                        [
                            { FunctionDef.generatedDef with
                                FuncDef =
                                  { Flags = Flags.Public
                                    Identifier = Identifier.ofString "equals"
                                    Position = pos }
                                Function =
                                    { Body = [] // TODO: Create the body here
                                      Parameters = [ [ { Name = "obj"; Type = Inferred } ] ]
                                      ReturnType = Primitive PrimitiveType.Boolean } }
                            |> Method
                        ])

                preturn (fun _ _ -> [])
            ]
        .>> typeDef "class"
        |> attempt
        .>>. genericDefinition // TODO: Validate the class name here with userStateSatisfies.
        .>> ignored
        .>>. classCtor
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

                semicolon >>% (emptyMembers, [])
            ]
        <??> "class definition"
        |>> fun (((((recordMembers, def), ctor), (superclass, baseArgs)), iimpls), (members, body)) ->
            let primaryCtor = ctor body baseArgs
            { TypeDef = def
              Header = Class
                { PrimaryCtor = primaryCtor
                  SuperClass = superclass }
              InitBody = body
              Interfaces = iimpls
              Members =
                match recordMembers primaryCtor.Parameters def.Position with
                | [] -> members
                | others -> members.Union(others) }

    let moduleDef =
        typeDef "module"
        >>. position
        .>>. identifierStr
        .>>. getUserState
        .>> ignored
        .>>. memberBlockInit
            [
                functionDef Function <?> "function definition"
                nestedTypes
            ]
        <??> "module definition"
        |>> fun (((pos, name), state), (members, body)) ->
            { TypeDef =
                Identifier.ofString name
                |> Definition.ofIdentifier state pos
              Header = Module
              InitBody = body
              Interfaces = []
              Members = members }

    nestedTypesRef :=
        [
            classDef <??> "nested class"
            interfaceDef <?> "nested interface"
            moduleDef <?> "nested module"
        ]
        |> choice
        |>> NestedType

    updateUserState newMembers
    >>. ignored
    >>. optList
        (skipString "namespace"
        >>. ignored1
        |> attempt
        >>. sepBy1 identifierStr (separator period)
        .>> ignored
        .>> semicolon
        <?> "namespace declaration"
        >>= fun names ->
            GlobalsTable.addNamespace names
            |> updateSymbols
            |> updateUserState
            >>% names)
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
    .>>. (choice
             [
                 classDef
                 interfaceDef
                 moduleDef
             ]
         |> (accessed Flags.Internal >> attempt)
         .>> ignored
         |> many1)
    .>> eof
    .>> updateUserState clearAllFlags
    >>= fun ((ns, uses), defs) ->
        // TODO: Figure out how to fail when a type with a duplicate name is parsed.
        GlobalsTable.addNamespace ns
        >> GlobalsTable.addTypes
            (Seq.map (GlobalTypeSymbol.ofTypeDef ns) defs)
            ns
        |> updateSymbols
        |> updateUserState
        >>%
        { Definitions = defs
          Namespace = ns
          Usings = uses }
