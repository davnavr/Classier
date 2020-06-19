module Classier.NET.Compiler.Parser

open System
open System.Collections.Immutable
open FParsec
open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.Identifier
open Classier.NET.Compiler.ParserState
open Classier.NET.Compiler.TypeSystem

let private position: Parser<_, ParserState> = fun stream -> Reply(stream.Position)

let private optList p = opt p |>> Option.defaultValue []

let debugIt p =
    tuple3
        p
        getUserState
        position
    >>= fun (result, state, pos) ->
        preturn result // Put a breakpoint here!

let private tuple6 p1 p2 p3 p4 p5 p6 =
    tuple5 p1 p2 p3 p4 p5 .>>. p6
    |>> (fun ((a, b, c, d, e), f) -> a, b, c, d, e, f)
let private tuple7 p1 p2 p3 p4 p5 p6 p7 =
    tuple6 p1 p2 p3 p4 p5 p6 .>>. p7
    |>> (fun ((a, b, c, d, e, f), g) -> a, b, c, d, e, f, g)
let private tuple8 p1 p2 p3 p4 p5 p6 p7 p8 =
    tuple7 p1 p2 p3 p4 p5 p6 p7 .>>. p8
    |>> (fun ((a, b, c, d, e, f, g), h) -> a, b, c, d, e, f, g, h)

let colon = skipChar ':'
let comma = skipChar ','
let gtsign = skipChar '>'
let lcurlybracket = skipChar '{' <?> "opening bracket"
let lparen = skipChar '(' <?> "opening parenthesis"
let ltsign = skipChar '<'
let period = skipChar '.' <?> "period"
let rcurlybracket = skipChar '}' <?> "closing bracket"
let rparen = skipChar ')' <?> "closing parenthesis"
let semicolon = skipChar ';' <?> "semicolon"
// TODO: Make a common function for lambdaOperator >>. (statementBlock <|> (expression .>> semicolon))
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

            [
                colon
                lcurlybracket
                lparen
                ltsign
                semicolon
            ]
            |> choice
            |> followedBy
        ]

let private separator p = space >>. attempt p .>> space

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
let typeNameOpt =
    choice
        [
            skipChar '_' >>% None
            typeName |>> Some
        ]

let typeAnn tname =
    space
    >>. colon
    |> attempt
    >>. space
    >>. tname
    <?> "type annotation"
let typeAnnOpt =
    typeNameOpt
    |> typeAnn
    |> attempt
    <|>% None
let typeAnnExp = typeAnn typeName

let modifiers =
    let modifier =
        [
            "abstract"
            "extern"
            "inheritable"
            "inline"
            "mutator"
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
let private validateModifiers modfs validator initial =
    let results =
        Seq.fold
            (fun prev modf ->
                match prev with
                | Result.Error _ -> prev
                | Result.Ok prevList ->
                    match validator prevList modf with
                    | Result.Ok result -> Result.Ok result
                    | Result.Error msg -> Result.Error (msg, modf))
            (Result.Ok initial)
            modfs
    match results with
    | Result.Ok resultList -> preturn resultList
    | Result.Error (err, modf) ->
        match err with
        | Some msg -> fail msg
        | None ->
            modf
            |> sprintf "The modifier '%s' is not valid here"
            |> fail
let private noModifiers modfs =
    validateModifiers
        modfs
        (fun _ _ -> Result.Error None)
        ()
let private badModfier str = str |> Some |> Result.Error

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
    asciiLetter
    .>>. manyChars (asciiLetter <|> pchar '_' <|> digit)
    |>> (String.Concat >> IdentifierStr)
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
    keyword "extends"
    >>. identifierFull
    |> opt
let implements =
    keyword "implements"
    >>. sepBy1
        identifierFull
        (separator comma)
    |> optList
    <?> "interface implementations"

// TODO: Parameters should check for duplicates in the same tuple, and for conflicts with the current self identifier.
let paramIdentifier = identifierStr >>= tryPushParam
let param typeAnn =
    paramIdentifier
    .>>. typeAnn
    <?> "parameter"
    |>> fun (name, ptype) ->
        { Name =
            match string name with
            | "_" -> None
            | _ -> Some name
          Type = ptype }
let paramTuple typeAnn =
    lparen
    |> attempt
    .>> space
    >>. sepBy (param typeAnn) (separator comma)
    .>> rparen
    <?> "parameters"
let paramTupleList typeAnn =
    paramTuple typeAnn
    .>> space
    |> many1
    <?> "parameter list"

let expressionRef = OperatorPrecedenceParser<_,_,_>()
let expression = expressionRef.ExpressionParser <?> "expression"
let equalsExpr =
    skipChar '='
    |> attempt
    >>. space
    >>. expression

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
        .>>. typeAnnOpt
        |>> VarPattern
        <?> "variable pattern"

        typeAnnOpt
        |> paramTuple
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
            tuple2
                position
                expression
            |>> fun (pos, expr) -> [ pos, Return expr ]

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
                        >>. position
                        .>>. ifStatement
                        |> attempt
                        |>> fun (pos, e) -> [ pos, IfStatement e ]

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
        simpleType
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
    statementRef :=
        position
        .>>. choiceL
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
                        | VarPattern (_, vtype) ->
                            [
                                value

                                paramTupleList typeAnnOpt
                                .>> space
                                .>>. typeAnnOpt
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
    |> attempt
    .>> space
    .>>. genericParams
    .>> space
    |>> fun (name, gparams) ->
        { Name = name
          Generics = List.map GenericParam gparams }
let genericName =
    tuple2
        position
        genericIdentifier
    .>> space
    |>> fun (pos, id) -> { Identifier = id; Position = pos }
let private genericTypeName placeholder =
    genericName
    >>= fun name ->
        let def = 
            name
            |> placeholder
            |> Type
        (Access.Public, def)
        |> tryAddMember
        >>% name

let opName: Parser<_, ParserState> =
    [ '!'; '%'; '&'; '*'; '+'; '-'; '/'; '<'; '='; '<'; '?'; '|'; '~' ]
    |> anyOf
    |> many1Chars

let selfId = 
    paramIdentifier
    .>> space
    .>> period
    .>> space
    <?> "self identifier"
    |> attempt
    |> opt
let selfIdAs =
    keyword "as"
    >>. paramIdentifier
    .>> space
    <?> "self identifier"
    |> opt

let functionBody =
    choiceL
        [
            statementBlock
            
            lambdaOperator
            |> attempt
            >>. space
            >>. position
            .>>. expression
            |>> fun (pos, e) -> [ pos, Return e ]
            .>> space
            .>> semicolon
        ]
        "function body"

let ctorDef modfs =
    let ctorHeader =
        paramTuple typeAnnOpt
        .>> space
        <?> "constructor parameters"
        >>= fun cparams ->
            let placeholder =
                { BaseCall = SuperCall List.empty
                  Body = List.empty
                  Parameters = cparams }
                |> Ctor
            tryAddMember (Access.Public, placeholder) >>% cparams
    let ctorBody ctorBase =
        [
            lambdaOperator
            |> attempt
            >>. space
            >>. ctorBase
            |>> fun bcall -> bcall, List.empty

            ctorBase <|>% SuperCall List.empty
            .>>. many (attempt statement .>> space)
            |> block
        ]
        |> choice

    keyword "new"
    |> attempt
    >>. noModifiers
        modfs
    >>. updateUserState newParams
    >>. ctorHeader
    .>>. selfIdAs
    >>= fun (cparams, selfid) -> // TODO: Come up with a way to keep track of the current self identifier. This will help with not only ctorDef but also in Expression parsing if a new case called Self is added.
        [
            keyword "super" >>% SuperCall
            keyword "this" >>% SelfCall
        ]
        |> choice
        .>>. tupleExpr
        |>> (fun (baseCall, baseArgs) -> baseCall baseArgs)
        <?> "base call"
        |> ctorBody
        |>> fun (baseCall, body) ->
            { BaseCall = baseCall
              Body = body
              Parameters = cparams }
            |> Ctor
    .>> tryPopParams
    <?> "constructor definition"

let functionDef modfs =
    let funcHeader =
        genericName
        .>>. paramTupleList typeAnnOpt
        .>> space
        >>= fun (name, fparams) ->
            let placeholder =
                {| Function =
                    { Body = List.empty
                      Parameters = fparams
                      ReturnType = None }
                   FunctionName = name |}
                |> Function
            tryAddMember (Access.Public, placeholder) >>% (name, fparams)
    noModifiers modfs
    .>> updateUserState newParams
    >>. tuple3
        funcHeader
        (typeAnnOpt .>> space)
        functionBody
    .>> tryPopParams
    |>> fun ((name, fparams), retType, body) ->
        Function
            {| Function =
                { Body = body
                  Parameters = fparams
                  ReturnType = retType }
               FunctionName = name |}
let methodDef modfs =
    let methodModf =
        validateModifiers
            modfs
            (fun (prev, isAbstract) modf ->
                match modf with
                | "abstract" ->
                    match prev.ImplKind with
                    | MethodImpl.SealedOverride ->
                        badModfier "A 'sealed' method cannot also be 'abstract'"
                    | MethodImpl.Virtual ->
                        badModfier "An 'abstract' method implies that it can be overriden, making the 'virtual' modifier redundant"
                    | _ -> Result.Ok (prev, true)
                | "mutator" -> Result.Ok ({ prev with IsMutator = true }, isAbstract)
                | "override" ->
                    match prev.ImplKind with
                    | MethodImpl.Virtual ->
                        badModfier "A 'virtual' method makes the 'override' modifier redundant, since it already can be overriden"
                    | _ ->
                        Result.Ok ({ prev with ImplKind = MethodImpl.Override }, false)
                | "sealed" ->
                    match prev.ImplKind with
                    | MethodImpl.Default ->
                        badModfier "The 'sealed' modifier is redundant, since methods cannot be overriden by default"
                    | MethodImpl.Virtual ->
                        badModfier "The method cannot be 'sealed' since it is already 'virtual'"
                    | _ when isAbstract -> badModfier "An abstract method cannot also be 'sealed'"
                    | _ ->
                        Result.Ok ({ prev with ImplKind = MethodImpl.SealedOverride}, false)
                | "virtual" ->
                    match prev.ImplKind with
                    | MethodImpl.Override ->
                        badModfier "An overriden method cannot be 'virtual'"
                    | MethodImpl.SealedOverride ->
                        badModfier "A sealed method cannot be 'virtual', since it cannot be overriden"
                    | _ when isAbstract -> badModfier "An abstract method already implies that the method is 'virtual'"
                    | _ -> Result.Ok ({ prev with ImplKind = MethodImpl.Virtual }, false)
                | _ -> Result.Error None)
            (MethodModifiers.Default, false)
    methodModf
    .>> updateUserState newParams
    >>= fun (mmodf, isAbstract) ->
        if isAbstract then
            tuple3
                genericName
                (paramTupleList typeAnnExp .>> space)
                (typeAnnExp <?> "method return type")
            >>= fun (name, mparams, retType) ->
                let memdef =
                    {| IsMutator = mmodf.IsMutator
                       IsOverride =
                         match mmodf.ImplKind with
                         | MethodImpl.Override -> true
                         | _ -> false
                       Method =
                           { Body = ()
                             Parameters = mparams
                             ReturnType = retType }
                           : Function<unit, TypeName>
                       MethodName = name |}
                    |> AbMethod
                tryAddMember (Access.Public, memdef)
                >>. space
                >>. semicolon
                >>% memdef
        else
            tuple3
                selfId
                genericName
                (paramTupleList typeAnnOpt)
            >>= fun (selfId, name, mparams) ->
                let placeholder =
                    MemberDef.placeholderMethod
                        name
                        selfId
                        mparams
                tryAddMember (Access.Public, placeholder)
                >>. typeAnnOpt
                .>> space
                .>>. functionBody
                |>> fun (retType, body) ->
                    {| Method =
                         { Body = body
                           Parameters = mparams
                           ReturnType = retType }
                       MethodName = name
                       Modifiers = mmodf
                       SelfIdentifier = selfId |}
                    |> Method
    .>> tryPopParams
    <?> "method definition"

let propDef modfs =
    let propModf =
        validateModifiers
            modfs
            (fun _ modf ->
                match modf with
                | "abstract" -> Result.Ok true
                | _ -> Result.Error None)
            false
    let propName =
        genericName
        >>= fun name ->
            let placeholder =
                {| Accessors = AutoGet
                   PropName = name
                   SelfIdentifier = None
                   Value = None
                   ValueType = None |}
                |> Property
            (Access.Public, placeholder)
            |> tryAddMember
            >>% name
    propModf
    >>= fun isAbstract ->
        if isAbstract then
            let body =
                keyword "get"
                >>. semicolon
                >>. space
                >>. choice
                    [
                        keyword "set"
                        >>. semicolon
                        >>% true

                        preturn false
                    ]
                |> block
            tuple3
                propName
                (typeAnnExp .>> space)
                body
            |>> fun (name, vtype, hasSet) ->
                {| HasSet = hasSet
                   PropName = name
                   ValueType = vtype |}
                |> AbProperty
        else
            let propBody =
                let setFull =
                    keyword "set"
                    |> attempt
                    >>. updateUserState newParams
                    >>. lparen
                    >>. space
                    >>. param typeAnnOpt
                    .>> space
                    .>> rparen
                    .>> tryPopParams
                    .>> space
                    .>>. functionBody
                    |> opt
                    <?> "set accessor"
                [
                    keyword "get"
                    >>. choice
                        [
                            semicolon
                            |> attempt
                            >>. space
                            >>. choice
                                [
                                    keyword "set"
                                    |> attempt
                                    >>. semicolon
                                    >>% AutoGetSet

                                    preturn AutoGet
                                ]

                            functionBody
                            |> attempt
                            .>> space
                            .>>. setFull
                            |>> fun (get, setOpt) ->
                                match setOpt with
                                | Some (sparam, set) -> GetSet (get, sparam, set)
                                | None -> Get get
                        ]
                    |> block
                    <?> "get accessor"

                    lambdaOperator
                    |> attempt
                    >>. space
                    >>. position
                    .>>. expression
                    .>> space
                    .>> semicolon
                    |>> fun (pos, expr) -> Get [ pos, Return expr ]
                ]
                |> choice
            let propValue =
                space
                >>. equalsExpr
                |> attempt
                |> opt
            tuple5
                selfId
                propName
                (typeAnnOpt .>> space)
                propBody
                propValue
            |>> fun (selfid, name, vtype, accessors, pvalue) ->
                {| Accessors = accessors
                   PropName = name
                   SelfIdentifier = selfid
                   Value = pvalue
                   ValueType = vtype |}
                |> Property
    <?> "property definition"

let memberDef members types =
    modifiers
    >>= fun modfs ->
        let memberDefs =
            Seq.map
                (fun def ->
                    modfs
                    |> def
                    |> attempt
                    .>> space)
                members
        [
            keyword "def"
            >>. choice memberDefs
            <?> "member"

            types
            |> Seq.map (fun def ->
                def modfs
                |>> Type
                .>> space)
            |> choice
        ]
        |> choice
let memberBlock (members: seq<_>) types =
    blockChoice
        [
            statement
            |> attempt
            |>> Some

            accessModifier Access.Private
            .>>. memberDef members types
            |>> (fun _ -> None)
        ]
    |>> List.choose id
    .>>. (getUserState |>> getMembers)
    |> memberSection

let private interfaceMembers, private interfaceMembersRef = createParserForwardedToRef<_, _>()
let interfaceDef modfs =
    keyword "interface"
    >>. noModifiers modfs
    >>. tuple3
        (genericTypeName TypeDef.placeholderInterface)
        (implements .>> space)
        interfaceMembers
    <?> "interface definition"
    |>> fun (name, ilist, members) ->
        Interface
            {| InterfaceName = name
               Members = members
               SuperInterfaces = ilist |}
do
    let members =
        [
            methodDef
            propDef
        ]
        |> Seq.map
            (fun def ->
                fun (rest: ImmutableList<_>) ->
                    "abstract"
                    |> rest.Add
                    |> def)
    interfaceMembersRef :=
        accessModifier Access.Internal
        >>. memberDef
            members
            [
                interfaceDef
            ]
        |> attempt
        .>> space
        |> many
        >>. (getUserState |>> getMembers)
        |> memberSection
        |> block
        <?> "interface body"

let private classBody, private classBodyRef = createParserForwardedToRef<_, _>()
let classDef modfs =
    let dataMembers =
        keyword "data"
        >>. position
        |>> fun pos ->
            fun values ->
                let name str pos =
                    { Identifier = Identifier.ofStr str
                      Position = pos }
                let selfid = "this__"
                seq {
                    Method
                        {| Method =
                            { Body = List.empty // TODO: Create a body here.
                              Parameters =
                                [
                                    [
                                        "obj"
                                        |> IdentifierStr 
                                        |> Some
                                        |> Param<_>.Create None
                                    ]
                                ]
                              ReturnType =
                                PrimitiveType.Boolean
                                |> TypeName.Primitive
                                |> Some }
                           MethodName = "equals" |> IdentifierStr |> Name.OfStr pos
                           Modifiers = MethodModifiers.Default
                           SelfIdentifier = IdentifierStr selfid |> Some |}

                    yield!
                        values
                        |> Seq.map
                            (fun (param: InfParam) ->
                                match param.Name with
                                | None -> None
                                | Some pname ->
                                    {| Accessors =
                                         pname
                                         |> Identifier.ofStr
                                         |> IdentifierRef
                                         |> Return
                                         |> Expression.withPos pos
                                         |> List.singleton
                                         |> Get
                                       PropName = Name.OfStr pos pname
                                       SelfIdentifier = None
                                       Value = None
                                       ValueType = param.Type |}
                                    |> Property
                                    |> Some)
                        |> Seq.choose id
                }
                |> Seq.map (fun mdef -> Access.Public, mdef)
        |> opt
    let classModf =
        validateModifiers
            modfs
            (fun inheritKind modf ->
                match (inheritKind, modf) with
                | (MustInherit, "inheritable")
                | (CanInherit, "abstract") ->
                    badModfier "An abstract class already implies that it is 'inheritable'"
                | (_, "inheritable") -> Result.Ok CanInherit
                | (_, "abstract") -> Result.Ok MustInherit
                | _ -> Result.Error None)
            Sealed
    let classCtor =
        let cparams =
            paramTuple typeAnnOpt
            .>> space
            |> optList
        accessModifier Access.Private
        .>> space
        |> opt
        |>> Option.defaultValue Access.Public
        .>>. cparams
        <?> "primary constructor"
        |>> fun (acc, ctorParams) ->
            fun body baseArgs -> // TODO: Add the primary constructor to the member set.
                let ctor =
                    { BaseCall = SuperCall baseArgs
                      Body = body
                      Parameters = ctorParams }
                acc, ctor
    let classBase =
        extends
        |> attempt
        .>> space
        .>>. optList tupleExpr
        .>> space
    let def header label body =
        tuple8
            header
            classModf
            (genericTypeName TypeDef.placeholderClass)
            classCtor
            classBase
            (implements .>> space)
            (selfIdAs |>> Option.defaultValue defaultSelfId)
            body
        <?> label
    [
        [
            semicolon >>% (List.empty, MemberDef.emptyMemberSet)
            classBody
        ]
        |> choice
        |> def
            (dataMembers .>> keyword "class")
            "record definition"

        def
            (keyword "class" >>% None)
            "class definition"
            classBody
    ]
    |> choice
    |>> fun (rmembers, cmodf, name, ctor, (sclass, superCall), ilist, selfid, (body, cmembers)) ->
        let primaryCtor = ctor body superCall
        Class
            {| ClassName = name
               Body = body
               Inheritance = cmodf
               Interfaces = ilist
               Members =
                 match rmembers with
                 | Some recordMems ->
                    (snd primaryCtor).Parameters
                    |> recordMems
                    |> cmembers.Union
                 | None -> cmembers
               PrimaryCtor = primaryCtor
               SelfIdentifier = selfid
               SuperClass = sclass |}
do
    classBodyRef :=
        memberBlock
            [
                ctorDef
                methodDef
                propDef
            ]
            [
                classDef
                interfaceDef
            ]
        <?> "class body"

let private moduleBody, private moduleBodyRef = createParserForwardedToRef<_, _>()
let moduleDef modfs =
    keyword "module"
    >>. noModifiers modfs
    >>. tuple2
        (genericTypeName TypeDef.placeholderModule)
        moduleBody
    <?> "module definition"
    |>> fun (name, (body, members)) ->
        Module
            {| Body = body
               ModuleName = name
               Members = members |}
do
    moduleBodyRef :=
        memberBlock
            [
                functionDef
            ]
            [
                classDef
                interfaceDef
                moduleDef
            ]
        <?> "module block"

do
    let toOp op = op :> Operator<_,_,_>
    let prefixOp op prec =
        (op, space, prec, true, fun exp -> PrefixOp (op, exp))
        |> PrefixOperator<_,_,_>
        |> toOp
    let infixOp op prec =
        (op, space, prec, Associativity.Left, fun e1 e2 -> InfixOp (e1, op, e2))
        |> InfixOperator<_,_,_>
        |> toOp
    let varAssignment target value =
        VarAssignment
            {| Target = target
               Value = value |}

    [
        infixOp "|>" 20
        infixOp ">>" 20
        infixOp ">>=" 20
        infixOp "||" 33
        infixOp "&&" 34
        infixOp "|" 35
        infixOp "^" 36
        infixOp "&" 37
        infixOp "==" 38
        infixOp "!=" 38
        infixOp ">=" 39
        infixOp "<=" 39
        infixOp "+" 40
        infixOp "-" 40
        infixOp "*" 50
        infixOp "/" 50
        prefixOp "-" 60
        prefixOp "!" 70

        ("<-", space, 100, Associativity.Right, varAssignment)
        |> InfixOperator<_,_,_>
        |> toOp
    ]
    |> Seq.iter expressionRef.AddOperator

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
        let numDigits chars =
            stringsSepBy1
                (anyOf chars |> many1Chars)
                (pchar '_' <?> "digit separator" |> many1Chars)
        let decimalChars = [ '0'..'9' ]
        let decimalDigits = numDigits decimalChars
        let integralSuffix =
            choice
                [
                    skipStringCI "L" >>% NumericLit.Long
                    preturn NumericLit.Integral
                ]
        let fpointSuffix =
            choice
                [
                    skipStringCI "F" >>% NumericLit.Float
                    skipStringCI "D" >>% NumericLit.Double
                    preturn NumericLit.FPoint
                ]
        let nonDecimal s nbase digits =
            skipChar '0'
            >>. skipStringCI s
            |> attempt
            >>. numDigits digits
            .>>. integralSuffix
            |>> fun (digits, itype) ->
                { Base = nbase
                  Digits = digits.ToUpper() }
                |> itype
        choiceL
            [
                nonDecimal
                    "b"
                    NumBase.Binary
                    [ '0'; '1' ]
                <?> "binary literal"

                [
                    decimalChars
                    [ 'a'..'z' ]
                    [ 'A'..'Z' ]
                ]
                |> Seq.collect List.toSeq
                |> nonDecimal "x" NumBase.Hexadecimal
                <?> "hexadecimal literal"

                decimalDigits
                >>= fun idigits ->
                    [
                        skipChar '.'
                        >>. decimalDigits
                        .>>. fpointSuffix
                        |>> fun (ddigits, dtype) ->
                            { IntDigits = idigits
                              FracDigits = ddigits }
                            |> dtype

                        integralSuffix
                        |>> fun itype ->
                            { Base = NumBase.Decimal
                              Digits = idigits }
                            |> itype
                    ]
                    |> choice
            ]
            "numeric literal"

    let simpleExpression =
        choice
            [
                strLit

                updateUserState newParams
                >>. paramTuple typeAnnOpt
                .>> space
                .>> lambdaOperator
                |> attempt
                .>> space
                .>>. position
                .>>. expression
                .>> tryPopParams
                <?> "anonymous function"
                |>> fun ((parameters, pos), retVal) ->
                    { Body = [ pos, Return retVal ]
                      Parameters = [ parameters ]
                      ReturnType = None }
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

    expressionRef.TermParser <- simpleExpression

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
            let nsName =
                Option.bind
                    Identifier.ofStrSeq
                    names
            GlobalsTable.addNamespace
                (Namespace.fullId nsName)
            |> updateSymbols
            >> setNamespace nsName
            |> updateUserState
            >>% nsName
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
        let entrypoint =
            let mainDef =
                position
                .>>. getUserState
                .>> skipString "main"
                |> attempt
                >>= fun (pos, state) ->
                    match state.EntryPoint with
                    | Some existing ->
                        existing.Origin
                        |> string
                        |> sprintf "An existing entry point was found at %s"
                        |> fail
                    | None ->
                        preturn (pos, state)
                .>> space
            let mainParams =
                updateUserState newParams
                >>. paramTuple typeAnnExp
                .>> space
                .>> tryPopParams
            tuple3
                mainDef
                mainParams
                functionBody
            <?> "entry point"
            >>= fun ((pos, state), eparams, body) ->
                { state with
                    EntryPoint =
                      { Body = body
                        Origin = pos
                        Parameters = eparams }
                      |> Some }
                |> setUserState
        [
            accessModifier Access.Internal
            .>>. typeDef
            >>= (replacePlaceholder >> updateUserState)
            |>> ignore

            entrypoint
        ]
        |> choice
        .>> space
        |> many

    (newMembers >> pushValidator typeValidator >> pushSelfId None >> newParams)
    |> updateUserState
    >>. space
    >>. namespaceDecl
    .>> space
    .>>. useStatements
    .>> definitions
    .>> eof
    .>>. getUserState
    .>> tryPopMembers
    .>> tryPopValidators
    .>> tryPopSelfId
    .>> tryPopParams
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
