module Classier.NET.Compiler.Parser

open System
open FParsec
open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.Identifier
open Classier.NET.Compiler.ParserState
open Classier.NET.Compiler.TypeSystem

let private position: Parser<Position, _> = fun stream -> Reply(stream.Position)

let private optList p = opt p |>> Option.defaultValue []

// TODO: Remove these single character parsers if they are only used once
let comma = skipChar ','
let gtsign = skipChar '>'
let lcurlybracket = skipChar '{' <?> "opening bracket"
let lparen = skipChar '(' <?> "opening parenthesis"
let ltsign = skipChar '<'
let period = skipChar '.' <?> "period"
let rcurlybracket = skipChar '}' <?> "closing bracket"
let rparen = skipChar ')' <?> "closing parenthesis"
let semicolon = skipChar ';' <?> "semicolon"
let lambdaOperator = skipString "=>" |> attempt <?> "lambda operator"

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

let keyword word =
    skipString word
    >>. space1
    |> attempt

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
                keyword str >>% vis
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

let typeName, private typeNameRef = createParserForwardedToRef<TypeName,_>()
let typeAnnotation =
    space
    >>. skipChar ':'
    |> attempt
    >>. space
    >>. typeName
    |> opt
    |>> Option.defaultValue Inferred
    <?> "type annotation"

let modifiers: Parser<string list, ParserState> = fail "not implemented"

let private separator p = space >>. attempt p .>> space

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

let statement, private statementRef = createParserForwardedToRef<Statement,_>()
let statementBlock = blockChoice [ statement ]

let genericArgs =
    ltsign
    >>. sepBy1 typeName (separator comma)
    |> attempt
    .>> gtsign
    |> optList
    <?> "generic arguments"

let identifierStr =
    let letterOrUnderscore =
        asciiLetter <|> pchar '_'
    letterOrUnderscore
    .>>. manyChars (letterOrUnderscore <|> digit)
    |>> String.Concat
let identifier =
    identifierStr
    .>> space
    .>>. genericArgs
    <?> "identifier"
    |>> fun (name, gparams) ->
        { Name = name
          Generics = List.map GenericArg gparams }
let identifierFull =
    sepBy1 identifier (separator period |> attempt)
    |>> FullIdentifier
    <?> "fully qualified name"

let extends =
    skipString "extends"
    .>> space1
    |> attempt
    >>. identifierFull
    |> opt
let implements =
    skipString "implements"
    .>> space1
    |> attempt
    >>. sepBy1 identifierFull (separator comma)
    |> optList
    <?> "interface implementations"

let paramTuple =
    let param =
        identifierStr
        .>>. typeAnnotation
        <?> "parameter"
        |>> fun (name, ptype) ->
            { Name = name
              Type = ptype }
    lparen
    |> attempt
    .>> space
    >>. sepBy param (separator comma)
    .>> rparen
    <?> "parameters"
let paramTupleList =
    paramTuple
    .>> space
    |> many1
    <?> "parameter list"

let expression, private expressionRef = createParserForwardedToRef<Expression, _>()
let tupleExpr =
    lparen
    |> attempt
    >>. space
    >>. sepBy expression (separator comma)
    .>> rparen
    <?> "tuple"
let parenExpr = tupleExpr |>> TupleLit

let pattern =
    [
        identifierStr
        .>> space
        .>>. typeAnnotation
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

            skipChar '_'
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
let matchStatement =
    skipString "match"
    >>. space
    >>. parenExpr
    .>> space
    .>>. matchCases
    |>> fun (against, cases) ->
        { Against = against
          Cases = cases }

let tryBlock =
    skipString "try"
    >>. space
    >>. statementBlock
    |> attempt
    <?> "try block"
    .>>. (space
        >>. skipString "catch"
        >>. space
        |> attempt
        >>. matchCases
        <?> "catch block"
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

// TODO: For parsers that rely on themselves, try to see if this works instead:
//let ifStatement =
//    let inner f =
//        // Call f inside here
//    inner inner
let ifStatement, private ifStatementRef = createParserForwardedToRef<If,_>()

do
    ifStatementRef :=
        skipString "if"
        >>. space1
        >>. parenExpr
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

do
    let simpleType =
        choiceL
            [
                PrimitiveType.names
                |> Seq.map (fun pair -> skipString (pair.Value) >>% pair.Key)
                |> choice
                |>> Primitive

                identifierFull |>> Identifier

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
            simpleType
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

do
    let equalsExpr =
        skipChar '='
        |> attempt
        >>. space
        >>. expression
    statementRef :=
        choiceL
            [
                semicolon >>% Empty <?> "empty statement"
            
                skipString "var"
                >>. space1
                |> attempt
                >>. pattern
                .>> space
                .>>. opt (equalsExpr .>> space .>> semicolon)
                <?> "mutable variable"
                |>> (Local.Var >> LocalVar)

                skipString "let"
                >>. space1
                |> attempt
                >>. pattern
                .>> space
                <?> "local variable or function"
                >>= fun p ->
                    let value =
                        equalsExpr
                        |> attempt
                        .>> space
                        .>> semicolon
                    let rest =
                        match p with
                        | VarPattern (name, vtype) ->
                            match vtype with
                            | Inferred ->
                                [
                                    value

                                    paramTupleList
                                    .>> space
                                    .>>. typeAnnotation
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
                        | _ -> None
                    rest
                    |> Option.defaultValue value
                    |>> fun value ->
                        (p, value)
                        |> Local.Let
                        |> LocalVar

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
let genericName =
    tuple2
        position
        genericIdentifier
    |>> fun (pos, id) -> { Identifier = id; Position = pos }

let functionBody =
    choiceL
        [
            statementBlock
            
            lambdaOperator
            |> attempt
            >>. space
            >>. expression
            |>> (Return >> List.singleton)
            .>> space
            .>> semicolon

            semicolon >>% []
        ]
        "function body"

let functionDef: Parser<unit, ParserState> = fail "func not implemented"
let methodDef: Parser<unit, ParserState> = fail "method not implemented"

let interfaceDef =
    keyword "interface"
    >>. genericName
    .>>. implements
    .>> space
    .>> updateUserState newMembers
    // .>>. blockChoice
    .>> fail "interface not implemented"

let classDef = fail "class not implemented"

let moduleDef = fail "module not implemented"

do
    let op = OperatorPrecedenceParser<_,_,_>()
    let expr = op.ExpressionParser <?> "expression"

    let callExpr target args name =
        match Identifier.ofString name with
        | None ->
            name
            |> sprintf "A function or method with the name %s is invalid"
            |> InvalidExpr
        | Some funcName ->
            FuncCall
                {| Arguments = args
                   Target = MemberAccess(target, funcName) |}

    [
        Seq.ofList<Operator<_,_,_>>
            [
                InfixOperator<_,_,_>(">>", space, 18, Associativity.Left, fun f g -> FuncComp (f, g))
                InfixOperator<_,_,_>("|>", space, 20, Associativity.Left, fun args f -> FuncCall {| Arguments = [ args ]; Target = f |})
                PrefixOperator<_,_,_>("!", space, 32, true, fun exp -> callExpr exp [] "not")
            ]
    ]
    |> Seq.collect id
    |> Seq.iter op.AddOperator

    op.TermParser <-
        choice
            [
            ]
        .>> space
        >>= fun target ->
            choice
                [
                    period
                    |> attempt
                    >>. space
                    >>. identifier
                    .>> space
                    <?> "member access"
                    |> many1
                    |>> fun members t ->
                            members
                            |> Seq.fold
                                (fun prev tmember -> MemberAccess (prev, tmember))
                                t

                    tupleExpr
                    .>> space
                    <?> "argument list"
                    |>> fun args t ->
                            match args with
                            | [] -> target
                            | _ -> FuncCall {| Arguments = args; Target = t |}
                ]
            .>> space
            |> many
            |>> Seq.fold
                (fun prev next -> next prev)
                target

    expressionRef := expr

let compilationUnit: Parser<CompilationUnit, ParserState> =
    let namespaceDecl =
        skipString "namespace"
        >>. space1
        |> attempt
        >>. sepBy1 identifierStr (separator period)
        .>> space
        .>> semicolon
        |> opt
        <?> "namespace declaration"
        >>= fun names ->
            let ns =
                names
                |> Option.defaultValue []
                |> Identifier.ofStrings
            GlobalsTable.addNamespace ns
            |> updateSymbols
            >> setNamespace ns
            |> updateUserState
            >>% ns
    let useStatements =
        skipString "use"
        >>. space1
        |> attempt
        >>. identifierFull
        .>> space
        .>> semicolon
        <?> "use statement"
        .>> space
        |> many
    let definitions =
        [
            accessModifier Access.Internal
            >>. choice
                [
                    classDef
                    interfaceDef
                    moduleDef
                ]
            |>> ignore

            skipString "main"
            |> attempt
            >>. space
            >>. paramTuple
            .>> space
            .>>. functionBody
            |>> ignore
            <?> "entry point"
        ]
        |> choice
        .>> space
        |> many1

    updateUserState newMembers
    >>. space
    >>. namespaceDecl
    .>> space
    .>>. useStatements
    .>> definitions
    .>> eof
    .>> tryPopMembers
    .>>. getUserState
    |>> fun ((ns, uses), state) ->
        { EntryPoint = state.EntryPoint
          Namespace = ns
          Usings = uses
          Types = invalidOp "not impl" }
