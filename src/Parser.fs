﻿module Classier.NET.Compiler.Parser

open System
open System.Collections.Immutable
open FParsec
open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.Identifier
open Classier.NET.Compiler.ParserState
open Classier.NET.Compiler.TypeSystem

let private position: Parser<Position, _> = fun stream -> Reply(stream.Position)

let private optList p = opt p |>> Option.defaultValue []

let private tuple6 p1 p2 p3 p4 p5 p6 =
    tuple5 p1 p2 p3 p4 p5 .>>. p6
    |>> (fun ((a, b, c, d, e), f) -> a, b, c, d, e, f)
let private tuple7 p1 p2 p3 p4 p5 p6 p7 =
    tuple6 p1 p2 p3 p4 p5 p6 .>>. p7
    |>> (fun ((a, b, c, d, e, f), g) -> a, b, c, d, e, f, g)

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
    pstring word
    .>> space1
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

let modifiers =
    let modifier =
        [
            "abstract"
            "data"
            "implicit"
            "inheritable"
            "inline"
            "override"
            "sealed"
            "virtual"
        ]
        |> Seq.map keyword
        |> choice
    (fun stream ->
        let results =
            Seq.unfold
                (fun _ ->
                    let result = modifier stream
                    match result.Status with
                    | Ok -> Some (result.Result, ())
                    | _ -> None)
                ()
            |> Seq.fold
                (fun state result ->
                    match state with
                    | Result.Ok (resultList: ImmutableList<_>) ->
                        if resultList.Contains result then
                            result
                            |> sprintf "Duplicate modifier %s"
                            |> Result.Error
                        else
                            result
                            |> resultList.Add
                            |> Result.Ok
                    | _ -> state)
                (Result.Ok ImmutableList.Empty)

        match results with
        | Result.Ok resultList -> Reply resultList
        | Result.Error err -> Reply(Error, messageError err))
    <?> "modifiers"
let private validateModifiers (modfs: seq<string>) validator initial =
    let results =
        modfs
        |> Seq.fold
            (fun prev modifier ->
                match prev with
                | Result.Error _ -> prev
                | Result.Ok prevList ->
                    match validator prevList modifier with
                    | Result.Ok result -> Result.Ok result
                    | _ -> Result.mapError id prev)
            (Result.Ok initial)
    match results with
    | Result.Ok resultList -> preturn resultList
    | Result.Error msg -> fail msg

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

let throwStatement =
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
    <?> "throw statement"

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
                        | VarPattern (name, vtype) -> // TODO: Is name needed here. Maybe simplify things by introducing a Statement case that represents a local functon?
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

                throwStatement |>> Throw
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
    .>> space1
    |>> fun (name, gparams) ->
        { Name = name
          Generics = List.map GenericParam gparams }
let genericName =
    tuple2
        position
        genericIdentifier
    .>> space
    |>> fun (pos, id) -> { Identifier = id; Position = pos }
let private genericNameValidated placeholder =
    genericName
    >>= fun name ->
        tryAddMember (placeholder name) >>% name

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
let methodDef modf =
    let methodHeader =
        choice
            [
                identifierStr
                |>> Some
                .>> period
                <?> "self identifier"
                |> attempt
                .>>. genericName

                preturn None .>>. genericName
            ]
        .>>. paramTupleList
        .>> space
        >>= fun ((selfId, name), mparams) ->
            let placeholder =
                MemberDef.placeholderMethod
                    name
                    selfId
                    mparams
            (Access.Public, placeholder)
            |> tryAddMember 
            >>% (selfId, name, mparams)

    tuple3
        methodHeader
        (typeAnnotation .>> space)
        functionBody
    |>> fun ((selfId, name, mparams), retType, body) ->
        Method
            {| Method =
                { Body = body
                  Parameters = mparams
                  ReturnType = retType }
               MethodName = name
               SelfIdentifier = selfId |}
let propDef modf = fail "not implemented"
let memberDefs members modfs =
    let defs =
        members
        |> Seq.map (fun def -> def modfs .>> space)
        |> choice

    keyword "def" >>. defs
let nestedTypes types modfs =
    types
    |> Seq.map (fun def ->
        def modfs
        |>> Type
        .>> space)
    |> choice

let interfaceDef modfs =
    keyword "interface"
    >>. genericName
    .>>. implements
    .>> space
    .>> updateUserState newMembers
    // .>>. blockChoice
    fail "interface not implemented"

let private classBody, private classBodyRef = createParserForwardedToRef<_, _>()
let classDef modfs =
    let classModf =
        validateModifiers
            modfs
            (fun inheritKind modf ->
                match (inheritKind, modf) with
                | (MustInherit, "inheritable")
                | (CanInherit, "abstract") -> Result.Error "An abstract class already implies that it is inheritable"
                | (_, "inheritable") -> Result.Ok CanInherit
                | (_, "abstract") -> Result.Ok MustInherit
                | _ ->
                    modf
                    |> sprintf "The modifier %s is not valid on a class"
                    |> Result.Error)
            (Sealed)
    let className =
        genericNameValidated
            (fun name ->
                let tdef =
                    name
                    |> TypeDef.placeholderClass
                    |> Type
                Access.Public, tdef)
    let classCtor =
        let cparams =
            paramTuple
            .>> space
            |> optList

        accessModifier Access.Private
        .>> space
        .>>. cparams
        |> attempt
        <?> "primary constructor"
        |>> fun (acc, ctorParams) ->
            fun body baseArgs ->
                let ctor =
                    { BaseCall = baseArgs
                      Body = body
                      Parameters = ctorParams }
                acc, ctor
    let classBase =
        extends
        |> attempt
        .>> space
        .>>. optList tupleExpr
        .>> space
    let classSelfId =
        keyword "as"
        |> attempt
        >>. identifierStr
        .>> space
        |> opt
        <?> "self identifier"
        |>> Option.defaultValue "this"
    keyword "class"
    >>. tuple7
        classModf
        className
        classCtor
        classBase
        (implements .>> space)
        classSelfId
        classBody
    <?> "class definition"
    |>> fun (cmodf, name, ctor, (sclass, superCall), ilist, selfid, (body, members)) ->
        Class
            {| ClassName = name
               Body = body
               Inheritance = cmodf
               Interfaces = ilist
               Members = members
               PrimaryCtor =
                 superCall
                 |> SuperCall
                 |> ctor body
               SelfIdentifier = selfid
               SuperClass = sclass |}

do
    let classMembers =
        modifiers
        >>= fun memberModfs ->
            [
                memberDefs
                    [
                        methodDef
                        propDef
                    ]

                nestedTypes
                    [
                        classDef
                        interfaceDef
                    ]
            ]
            |> Seq.map (fun def -> def memberModfs)
            |> choice
    classBodyRef :=
        (newMembers >> pushValidator memberValidator)
        |> updateUserState
        >>. blockChoice
            [
                statement
                |> attempt
                |>> Some

                accessModifier Access.Private
                .>>. classMembers
                |>> (fun _ -> None)
            ]
        <?> "class body"
        |>> List.choose id
        .>>. (getUserState |>> getMembers)
        .>> tryPopMembers
        .>> tryPopValidators

let moduleDef modfs = fail "module not implemented"

do
    let expr = OperatorPrecedenceParser<_,_,_>()

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
    |> Seq.iter expr.AddOperator

    let strLit =
        let quote = skipChar '\"' <?> "quotation mark"
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
            <?> "escaped character"

        between
            quote
            quote
            (stringsSepBy strChar strEscaped)
        |>> StrLit
        <?> "string literal"

    let numLit =
        let digits chars =
            let digitSep =
                skipChar '_'
                <?> "digit separator"
                |> skipMany
            let digit = anyOf chars

            attempt digit
            .>> digitSep
            .>>. opt digit
            |>> fun (d1, d2) ->
                match d2 with
                | Some two -> sprintf "%c%c" d1 two
                | None -> d1.ToString()
            |> many1Strings
        let decimalChars = [ '0'..'9' ]
        let hexChars =
            [
                decimalChars
                [ 'a'..'z' ]
                [ 'A'..'Z' ]
            ]
            |> List.collect id

        skipChar '-' >>% true <|>% false
        .>>. choice
            [
                skipChar '0'
                >>. choice
                    [
                        anyOf [ 'b'; 'B' ] >>% ([ '0'; '1' ], NumBase.Binary)
                        anyOf [ 'x'; 'X' ] >>% (hexChars, NumBase.Hexadecimal)
                    ]
                |> attempt

                period >>% (List.empty, NumBase.Decimal)

                preturn (decimalChars, NumBase.Decimal)
            ]
        >>= (fun (isNeg, (chars, nbase)) ->
            let decimalDigits = digits decimalChars
            let numParser =
                match chars with
                | [] ->
                    decimalDigits
                    |>> fun fdigits ->
                        String.Empty, fdigits
                | _ when chars.Length = 10 ->
                    let fraction =
                        skipChar '.'
                        |> attempt
                        >>. decimalDigits
                        |> opt
                        |>> Option.defaultValue String.Empty
                    decimalDigits .>>. fraction
                | _ ->
                    digits chars
                    |>> fun digits ->
                        digits, String.Empty
            numParser .>>. preturn (nbase, isNeg))
        >>= fun ((idigits, fdigits), (nbase, isNeg)) ->
            let suffixes pairs none =
                pairs
                |> Seq.map (fun (c, ntype) -> skipChar c >>% ntype)
                |> choice
                <|>% none
                <?> "numeric suffix"

            match fdigits with
            | "" ->
                suffixes
                    [ 'l', Long ]
                    Integral
                |>> fun intType ->
                    { Base = nbase
                      Digits = idigits
                      Negative = isNeg }
                    |> intType
            | _ ->
                suffixes
                    [
                        'd', Double
                        'f', Float
                    ]
                    Double
                |>> fun fpType ->
                    { IntDigits = idigits
                      FracDigits = fdigits
                      Negative = isNeg }
                    |> fpType

    expr.TermParser <-
        choice
            [
                strLit

                paramTuple
                .>> space
                .>> lambdaOperator
                |> attempt
                .>> space
                .>>. expression
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
                    | _ -> TupleLit ex)

                ifStatement |>> IfExpr <?> "if expression"
                matchStatement |>> MatchExpr <?> "match expression"
                tryBlock |>> TryExpr <?> "try expression"
                skipString "null" >>% NullLit <?> "null literal"

                throwStatement
                >>= fun ex ->
                    match ex with
                    | None -> fail "Throw statements used as expressions must provide an exception to throw"
                    | Some _ -> ex.Value |> ThrowExpr |> preturn

                skipString "new"
                >>. space
                |> attempt
                >>. identifierFull
                .>> space
                .>>. (tupleExpr <?> "constructor arguments")
                <?> "explicit constructor call"
                |>> (fun (ctype, args) ->
                    CtorCall
                        {| Arguments = args
                           Type = Identifier ctype |})

                [
                    pstring "true"
                    pstring "false"
                ]
                |> choice
                |>> (bool.Parse >> BoolLit)
                <?> "boolean literal"

                identifier |>> IdentifierRef
                numLit |>> NumLit
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

    expressionRef := expr.ExpressionParser <?> "expression"

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
        let typeDef =
            modifiers
            >>= (fun modfs ->
                [
                    classDef
                    interfaceDef
                    moduleDef
                ]
                |> Seq.map (fun def -> def modfs)
                |> choice)
        [
            accessModifier Access.Internal
            .>>. typeDef
            >>= (replacePlaceholder >> updateUserState)
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

    (newMembers >> pushValidator typeValidator)
    |> updateUserState
    >>. space
    >>. namespaceDecl
    .>> space
    .>>. useStatements
    .>> definitions
    .>> eof
    .>> tryPopMembers
    .>> tryPopValidators
    .>>. getUserState
    |>> fun ((ns, uses), state) ->
        { EntryPoint = state.EntryPoint
          Namespace = ns
          Usings = uses
          Types =
            state
            |> ParserState.getMembers
            |> Seq.choose
                (fun (acc, mdef) ->
                    match mdef with
                    | Type tdef -> Some (acc, tdef)
                    | _ -> None)}
