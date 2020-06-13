module Classier.NET.Compiler.Parser

open System
open FParsec
open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.ParserState
open Classier.NET.Compiler.TypeSystem

let private position: Parser<Position, _> = fun stream -> Reply(stream.Position)
let private optList p = opt p |>> Option.defaultValue []

// TODO: Remove these single character parsers if they are only used once
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

// TODO: Determine if forwarded parsers are needed for each one.
let identifier, private identifierRef = createParserForwardedToRef<Identifier,_>()
let ifStatement, private ifStatementRef = createParserForwardedToRef<If,_>()
let matchStatement, private matchStatementRef = createParserForwardedToRef<Match,_>()
let nestedTypes, private nestedTypesRef = createParserForwardedToRef<MemberDef,_>()
let tryBlock, private tryBlockRef = createParserForwardedToRef<Try,_>()
let tupleExpr, private tupleExprRef = createParserForwardedToRef<Expression list,_>()
let typeName, private typeNameRef = createParserForwardedToRef<TypeName,_>()
let statement, private statementRef = createParserForwardedToRef<Statement,_>()

let space =
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
let space1 =
    choice
        [
            notEmpty space
            followedBy lparen
        ]

let accessModifier lowestAccess =
    let modifiers =
        [
            "public", Access.Public
            "internal", Access.Internal
            "protected", Access.Protected
            "private", Access.Private
        ]
        |> Seq.map (fun (str, vis) ->
            if vis <= lowestAccess then
                skipString str
                .>> space1
                |> attempt
                >>% vis
            else
                vis
                |> sprintf "%A access is not valid here"
                |> fail)
    choice
        [
            choiceL
                modifiers
                "access modifier"
            |> attempt

            preturn Access.Public
        ]

// TODO: Clean up parser and put all the functions in the module.
let compilationUnit: Parser<CompilationUnit, ParserState> =
    let typeAnnotation =
        space
        >>. skipChar ':'
        |> attempt
        >>. space
        >>. typeName
        <?> "type annotation"
    let typeAnnotationOpt =
        typeAnnotation
        |> opt
        |>> Option.defaultValue Inferred

    let modifier name flag =
        skipString name
        >>. space1
        |> attempt
        >>. updateUserState (setFlags flag)
        |> optional
    let modifiers pairs =
        pairs
        |> Seq.map (fun (name, flag) -> modifier name flag)
        |> Seq.reduce (fun one two -> one >>. two)

    let separator p = space >>. attempt p .>> space

    let block p =
        lcurlybracket
        |> attempt
        >>. space
        >>. p
        .>> space
        .>> rcurlybracket
    let blockChoice p =
        choice p
        |> attempt
        .>> space
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
        .>> space
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
        .>> space
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
            InfixOperator<_,_,_>(op, space, prec, assoc, map))
        |> Seq.cast<Operator<_,_,_>>
        |> Seq.append
            [
                InfixOperator<_,_,_>(">>", space, 18, Associativity.Left, fun f g -> FuncComp (f, g));
                InfixOperator<_,_,_>("|>", space, 20, Associativity.Left, fun args f -> FuncCall {| Arguments = [ [ args ] ]; Target = f |});
                PrefixOperator<_,_,_>("!", space, 32, true, fun exp -> functionCall exp [] "not");
                PrefixOperator<_,_,_>("-", space, 60, true, fun exp -> functionCall exp [] "negate");
                InfixOperator<_,_,_>("<-", space, 100, Associativity.Right, assignment VarAssignment);
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
                    .>> space
                    .>> space
                    .>> lambdaOperator
                    |> attempt
                    .>> space
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
                    >>. space1
                    |> attempt
                    >>. exprParser
                    |>> ThrowExpr

                    skipString "new"
                    >>. space1
                    |> attempt
                    >>. opt identifierFull
                    .>> space
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
            .>> space
            >>= fun target ->
                let memberAccess =
                    period
                    |> attempt
                    >>. space
                    >>. identifier
                    .>> space
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
                    .>> space
                    |> many1 <?>
                    "argument list"
                    |>> fun args ->
                        fun t ->
                            match args with
                            | [] -> target
                            | _ -> FuncCall {| Arguments = args; Target = t |}
                memberAccess <|> funcCall
                .>> space
                |> many1
                <|>% []
                |>> Seq.fold
                    (fun prev next -> next prev)
                    target
        exprParser
    let expressionInParens = // NOTE: Tuples might not parse correctly here.
        lparen
        |> attempt
        >>. space
        >>. expression
        .>> rparen
    let equalsExpression =
        opequal
        |> attempt
        >>. space
        >>. expression

    let lambdaBody =
        lambdaOperator
        |> attempt
        >>. space
        >>. expression
        |>> Return
        |>> List.singleton
        .>> space
        .>> semicolon

    let pattern =
        [
            identifierStr
            .>> space
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
                >>. space
                |> attempt
                >>% Default
                <?> "default pattern"
            ]
            |> choice
        sepBy1 matchPattern (separator comma |> attempt)
        .>> space
        .>> lambdaOperator
        |> attempt
        .>> space
        .>>. choice
            [
                expression |>> (Return >> List.singleton)
                statementBlock
            ]
        .>> space
        .>> semicolon
        |>> (fun (patterns, body) -> { Body = body; Patterns = patterns })
        .>> space
        |> many1
        |> block
        <?> "cases"

    tupleExprRef :=
        lparen
        |> attempt
        >>. space
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
            (space
            >>. lambdaOperator
            >>. space
            >>. typeName
            |> attempt)
        |>> fun (paramsType, retType) ->
            match retType with
            | Some _ -> FuncType {| ParamType = paramsType; ReturnType = retType.Value |}
            | None -> paramsType

    identifierRef :=
        identifierStr
        .>> space
        .>>. genericArgs
        |>> fun (name, gparams) ->
            { Name = name
              Generics = List.map GenericArg gparams }

    ifStatementRef :=
        skipString "if"
        >>. ignored1OrSep
        >>. expressionInParens
        .>> space
        .>>. statementBlock
        |> attempt
        .>>. choice
            [
                space
                >>. skipString "else"
                |> attempt
                >>. choice
                    [
                        space1
                        >>. ifStatement
                        |> attempt
                        |>> fun e -> [ IfStatement e ]

                        space
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
        >>. space
        >>. expressionInParens
        .>> space
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
                .>> space
                .>>. opt (equalsExpression .>> space .>> semicolon)
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
                .>> space
                .>>. getUserState
                >>= (fun ((pos, p), state) ->
                    let value =
                        equalsExpression
                        |> attempt
                        .>> space
                        .>> semicolon
                    let rest =
                        (match p with
                        | VarPattern (name, vtype) ->
                            match vtype with
                            | Inferred ->
                                [
                                    value

                                    paramTupleList
                                    .>> space
                                    .>>. typeAnnotationOpt
                                    .>> space
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
                >>. space
                |> attempt
                >>. lparen
                >>. expression
                .>> rparen
                .>> space
                .>>. statementBlock
                |>> While
                <?> "while loop"

                skipString "return"
                >>. space1
                |> attempt
                >>. expression
                |>> Return
                <?> "return statement"
                .>> space
                .>> semicolon

                skipString "throw"
                >>. choice
                    [
                        space1
                        |> attempt
                        >>. expression
                        |>> Some

                        space
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
                .>> space
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
        >>. space
        >>. statementBlock
        |> attempt
        .>>. (space
            >>. skipString "catch"
            >>. space
            |> attempt
            >>. matchCases
            |> optList)
        .>>.
            (space
            >>. skipString "finally"
            >>. space
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
        .>> space1
        |> attempt
        >>. identifierList
        |> optList

    let implements =
        skipString "implements"
        .>> space1
        |> attempt
        >>. sepBy1 identifierFull (separator comma)
        |> optList
        <?> "interface implementations"

    let genericParams =
        let genericParam =
            choice
                [
                    skipString "in"
                    >>. space1
                    |> attempt
                    >>% Contravariant;

                    skipString "out"
                    >>. space1
                    |> attempt
                    >>% Covariant;

                    preturn NoVariance;
                ]
            .>>. identifierStr
            |> attempt
            .>> space
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
        .>> space
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
        .>> space
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
        >>. space1
        >>. functionHeader f
        .>>. typeAnnotationOpt
        .>> space
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
                "Valid modifier on abstract method" // TODO: Replace these with explicit calls to fail, since these are not used as proper error messages.
        >>. validateFlags
                (fun flags -> (flags &&& Flags.MethodImplMask).HasFlag(Flags.Sealed ||| Flags.Virtual))
                "Unsealed virtual method"
        >>. functionDef Method
        <?> "method definition"

    let typeDef word =
        skipString word
        >>. space1
        |> attempt

    let interfaceDef =
        typeDef "interface"
        >>. genericDefinition
        .>>. implements
        .>> space
        .>> updateUserState newMembers
        .>> blockChoice
            [
                [
                    methodDef
                    nestedTypes
                ]
                |> (choice >> accessModifier Flags.Internal)
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
                |> accessModifier Flags.Private
                |>> LocalMember // TODO: Use Choice<'T1,'T2> to parse either a statement or a member.
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
            optList (paramTuple .>> space)
            .>>. getUserState
            |> accessModifier Flags.Private
            |>> fun (ctorParams, state) ->
                let flags =
                    match ctorParams with
                    | [] -> Flags.Public
                    | _ -> currentFlags state
                fun body baseArgs ->
                    { BaseCall = SuperCall baseArgs
                      Body = body
                      Parameters = ctorParams }
        let classExtends =
            extends
            .>>. optList (tupleExpr .>> space)
            .>> space

        modifiers
            [
                "abstract", Flags.Abstract
                "inheritable", Flags.Inheritable
                "mutable", Flags.Mutable
            ]
        >>. choice
            [
                skipString "data"
                >>. space1
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
        .>>. genericDefinition // TODO: Validate the class name here with userStateSatisfies. Nested classes should check the Members stack while top-level classes should check the GlobalsTable.
        .>> space
        .>>. classCtor
        .>>. classExtends
        .>>. implements
        .>> space
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
        .>> space
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
        |>> Type

    updateUserState newMembers
    >>. space
    >>. optList
        (skipString "namespace"
        >>. space1
        |> attempt
        >>. sepBy1 identifierStr (separator period)
        .>> space
        .>> semicolon
        <?> "namespace declaration"
        >>= fun names ->
            GlobalsTable.addNamespace names
            |> updateSymbols
            |> updateUserState
            >>% names)
    .>> space
    .>>. many
        (skipString "use"
        >>. space1
        |> attempt
        >>. identifierList
        .>> space
        .>> semicolon
        <?> "use statement"
        .>> space)
    .>>. (choice
             [
                 classDef
                 interfaceDef
                 moduleDef
             ]
         |> (accessModifier Flags.Internal >> attempt)
         .>> space
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
