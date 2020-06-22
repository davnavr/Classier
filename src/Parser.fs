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
        preturn result

let private tuple6 p1 p2 p3 p4 p5 p6 =
    tuple5 p1 p2 p3 p4 p5 .>>. p6
    |>> (fun ((a, b, c, d, e), f) -> a, b, c, d, e, f)
let private tuple7 p1 p2 p3 p4 p5 p6 p7 =
    tuple6 p1 p2 p3 p4 p5 p6 .>>. p7
    |>> (fun ((a, b, c, d, e, f), g) -> a, b, c, d, e, f, g)

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

let paramIdentifier = identifierStr >>= tryAddParam ParamIdentifier
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

let lambdaBlock =
    lambdaOperator
    |> attempt
    >>. space
    >>. choice
        [
            tuple2
                position
                (expression |>> Return)
            |>> List.singleton

            statementBlock
        ]

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
    .>>. lambdaBlock
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
let simpleName: Parser<SimpleName, _> =
    position
    .>>. identifierStr
    |>> fun (pos, name) -> Name.simple pos name
let private genericTypeName placeholder =
    genericName
    >>= fun name ->
        name
        |> placeholder
        |> tryAddPlaceholder
        >>% name

let opName: Parser<_, ParserState> =
    Operator.operatorChars
    |> anyOf
    |> many1Chars

let selfIdStr = identifierStr >>= tryAddParam SelfIdentifier
let selfId =
    selfIdStr
    .>> space
    .>> period
    .>> space
    <?> "self identifier"
    |> attempt
    |> opt
let selfIdAs =
    keyword "as"
    >>. selfIdStr
    .>> space
    <?> "self identifier"
    |> opt


let functionBody =
    choiceL
        [
            statementBlock

            lambdaBlock
            .>> space
            .>> semicolon
        ]
        "function body"

let ctorDef cdef modfs =
    let ctorHeader =
        paramTuple typeAnnOpt
        .>> space
        <?> "constructor parameters"
        >>= fun cparams ->
            { Member.defaultCtor with Parameters = cparams }
            |> cdef
            |> tryAddPlaceholder
            >>% cparams
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
    .>>. (tryMapState getSelfId)
    >>= fun ((cparams, selfid), currentSelf) ->
        [
            keyword "super" >>% SuperCall

            currentSelf
            |> string
            |> keyword
            >>% SelfCall
        ]
        |> choice
        .>>. tupleExpr
        |>> (fun (baseCall, baseArgs) -> baseCall baseArgs)
        <?> "base call"
        |> ctorBody
        |>> fun (baseCall, body) ->
            { BaseCall = baseCall
              Body = body
              Parameters = cparams
              SelfIdentifier = selfid }
    .>> (tryUpdateState popParams)
    <?> "constructor definition"
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
                | "mutator" -> Result.Ok ({ prev with Purity = IsMutator }, isAbstract)
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
            .>> space
            .>> semicolon
            |>> fun (name, mparams, retType) ->
                { Method =
                     { Body = ()
                       Parameters = mparams
                       ReturnType = retType }
                     : Function<unit, TypeName>
                  MethodName = name }
                |> AMethod
                |> Abstract
                |> Member
        else
            tuple4
                selfId
                genericName
                (paramTupleList typeAnnOpt .>> space)
                typeAnnOpt
            .>> space
            >>= fun (selfId, name, mparams, retType) ->
                let mthd = Method >> Concrete >> Member
                let def =
                    { Method =
                        { Body = List.empty
                          Parameters = mparams
                          ReturnType = retType }
                      MethodName = name
                      Modifiers = mmodf
                      SelfIdentifier = selfId }
                tryAddPlaceholder (mthd def |> ClassMember)
                >>. functionBody
                |>> fun body ->
                    { def with
                        Method =
                          { def.Method with
                              Body = body } }
                    |> mthd
    .>> (tryUpdateState popParams)
    <?> "method definition"
    |> attempt

let propDef aprop cprop pdef modfs =
    let propModf =
        validateModifiers
            modfs
            (fun _ modf ->
                match modf with
                | "abstract" -> Result.Ok true
                | _ -> Result.Error None)
            false
    let propName def =
        simpleName
        >>= fun name ->
            def name
            |> tryAddPlaceholder
            >>% name
    propModf
    >>= fun isAbstract ->
        match cprop with
        | Some cprop when (not isAbstract) ->
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
                    .>> (tryUpdateState popParams)
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

                    lambdaBlock
                    |>> Get
                    |> attempt
                    .>> space
                    .>> semicolon
                ]
                |> choice
            let propValue =
                space
                >>. equalsExpr
                |> attempt
                |> opt
            tuple5
                selfId
                (propName
                    (fun name ->
                        { Accessors = AutoGet
                          PropName = name
                          SelfIdentifier = None
                          Value = None
                          ValueType = None }
                        |> cprop
                        |> pdef))
                (typeAnnOpt .>> space)
                propBody
                propValue
            |>> fun (selfid, name, vtype, accessors, pvalue) ->
                { Accessors = accessors
                  PropName = name
                  SelfIdentifier = selfid
                  Value = pvalue
                  ValueType = vtype }
                |> cprop
        | _ ->
            let body =
                keyword "get"
                >>. semicolon
                >>. space
                >>. choice
                    [
                        keyword "set"
                        >>. semicolon
                        >>% AbstractGetSet

                        preturn AbstractGet
                    ]
                |> block
            tuple3
                (propName
                    (fun name ->
                        { Accessors = AbstractGet
                          PropName = name
                          Purity = IsMutator
                          ValueType = TypeName.Unknown }
                        |> aprop
                        |> pdef))
                (typeAnnExp .>> space)
                body
            |>> fun (name, vtype, accessors) ->
                { Accessors = accessors
                  PropName = name
                  Purity = IsPure // TODO: Handle modifiers for properties.
                  ValueType = vtype }
                |> aprop
    <?> "property definition"
    |> attempt

let memberDef members types =
    modifiers
    >>= fun modfs ->
        let memberDefs =
            Seq.map
                (fun def -> def modfs .>> space)
                members
        [
            keyword "def"
            >>. choice memberDefs
            <?> "member"

            types
            |> Seq.map (fun def ->
                def modfs
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
let memberSection empty validator selector members =
    pushMembers
        empty
        (fun set mdef ->
            validator set mdef
            |> Option.bind (Option.map Result.Ok)
            |> Option.defaultValue (Result.Error "Invalid member or set type"))
    |> updateUserState
    >>. members
    >>. tryMembers selector
    .>> tryUpdateState popMembers
let memberSection2 empty setf setsel memsel mname members =
    pushMembers
        (setf empty)
        (fun (acc, mdef) set ->
            match setsel set with
            | Some actualSet ->
                match memsel mdef with
                | Some actualMember ->
                    match actualSet with
                    | SortedSet.Contains actualMember ->
                        actualMember
                        |> mname
                        |> sprintf "A member with the %s already exists"
                        |> Result.Error
                    | _ ->
                        actualSet
                        |> SortedSet.add actualMember
                        |> setf
                        |> Result.Ok
                | None ->
                    mdef
                    |> string
                    |> sprintf "The member %A cannot be added to the set since it is an invalid member type"
                    |> Result.Error
            | None -> Result.Error "Invalid member set type")
    |> updateUserState
    >>. members
    >>. tryMembers (fun _ -> invalidOp "badder") //selector
    .>> tryUpdateState popMembers

let private interfaceBody, private interfaceBodyRef = createParserForwardedToRef<_, _>()
let interfaceDef iface idef modfs =
    let interfaceName =
        genericTypeName
            (fun name ->
                { InterfaceName = name
                  Members = MemberSet.interfaceSet
                  SuperInterfaces = List.empty }
                |> iface
                |> idef)
    keyword "interface"
    >>. noModifiers modfs
    >>. tuple3
        interfaceName
        (implements .>> space)
        interfaceBody
    <?> "interface definition"
    |>> fun (name, ilist, members) ->
        { InterfaceName = name
          Members = members
          SuperInterfaces = ilist }
        |> iface
do
    let iset f set =
        match set with
        | InterfaceSet iset -> f iset |> Some
        | _ -> None
    let members =
        [
            propDef
                (AProperty >> Member)
                None
                InterfaceMember
            //methodDef
        ]
        |> Seq.map
            (fun def ->
                fun (rest: ImmutableList<_>) ->
                    "abstract"
                    |> rest.Add
                    |> def)
    interfaceBodyRef :=
        accessModifier Access.Internal
        >>. memberDef
            members
            [ interfaceDef Type InterfaceMember ]
        |> attempt
        .>> space
        |> many
        |> block
        |> memberSection // TODO: iset is used alot. Add another parameter to memberSection?
            (InterfaceSet MemberSet.interfaceSet)
            (fun (acc, mdef) ->
                (fun set ->
                    match mdef with
                    | InterfaceMember imem ->
                        set
                        |> SortedSet.tryAdd (acc, imem)
                        |> Option.map InterfaceSet
                    | _ -> None)
                |> iset)
            (iset id)
        <?> "interface body"

let private classBody, private classBodyRef = createParserForwardedToRef<_, _>()
let classDef cdef modfs: Parser<Class, _> =
    let dataMembers =
        keyword "data"
        >>. position
        |>> fun pos ->
            fun values ->
                let selfid = "this__"
                seq {
                    Method
                        { Method =
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
                          MethodName = "equals" |> IdentifierStr |> Name.ofStr pos
                          Modifiers = MethodModifiers.Default
                          SelfIdentifier = IdentifierStr selfid |> Some }

                    yield!
                        values
                        |> Seq.map
                            (fun (param: InfParam) ->
                                match param.Name with
                                | None -> None
                                | Some pname ->
                                    { Accessors =
                                        pname
                                        |> Identifier.ofStr
                                        |> IdentifierRef
                                        |> Return
                                        |> Expression.withPos pos
                                        |> List.singleton
                                        |> Get
                                      PropName = Name.simple pos pname
                                      SelfIdentifier = None
                                      Value = None
                                      ValueType = param.Type }
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
        accessModifier Access.Private
        .>> space
        |> opt
        |>> Option.defaultValue Access.Public
        .>>. paramTuple typeAnnOpt
        .>> space
        <?> "primary constructor"
        >>= fun (acc, ctorParams) ->
            let placeholder = { Member.defaultCtor with Parameters = ctorParams }
            let actual body baseArgs = // TODO: Don't forget to replace it in the member set when the actual primary ctor is retrieved.
                let ctor =
                    { placeholder with
                        BaseCall = SuperCall baseArgs
                        Body = body }
                acc, ctor
            tryAddMember (acc, Constructor placeholder)
            >>% actual
        |> opt
    let classBase =
        extends
        |> attempt
        .>> space
        .>>. opt tupleExpr
        .>> space
    let classSelf =
        selfIdAs
        >>= fun selfid ->
            match selfid with
            | Some self -> preturn self
            | None ->
                tryAddParam
                    SelfIdentifier
                    defaultSelfId
    let def header label body =
        let actualBody =
            updateUserState newParams
            >>. tuple5
                classCtor
                classBase
                (implements .>> space)
                classSelf
                body
            .>> (tryUpdateState popParams)
            |> memberSection
        tuple4
            header
            classModf
            (genericTypeName TypeDef.placeholderClass)
            actualBody
        <?> label
    [
        [
            semicolon >>% (List.empty, MemberSet.classSet)
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
    |>> fun (rmembers, cmodf, name, (ctorGen, (sclass, scall), ilist, selfid, (body, cmembers))) ->
        let def (members) pctor =
            { ClassName = name
              Body = body
              Inheritance = cmodf
              Interfaces = ilist
              Members =
                match pctor with
                | Some (acc, ctor) ->
                   let ctorDef =
                       ctor
                       |> Constructor
                       |> Member.withAccess acc
                   SortedSet.add ctorDef members
                | None -> members
              PrimaryCtor = pctor
              SelfIdentifier = selfid
              SuperClass = sclass }
        let noCtor =
            Seq.forall
                (fun (_, mdef)->
                    match mdef with
                    | Constructor _ -> false
                    | _ -> true)
        match (ctorGen, cmembers, scall) with
        | (Some ctor, _, _) ->
            let primaryCtor =
                scall
                |> Option.defaultValue List.empty
                |> ctor body
            let members =
                match rmembers with
                | Some recordMems ->
                   (snd primaryCtor).Parameters
                   |> recordMems
                   |> cmembers.Union
                | None -> cmembers
            def
                members
                (Some primaryCtor)
        | (None, _, Some args) ->
            { MemberDef.emptyCtor with
                BaseCall = SuperCall args
                Body = body }
            |> MemberDef.defaultAccess
            |> Some
            |> def cmembers
        | (None, _, _) when (noCtor cmembers) ->
            { MemberDef.emptyCtor with
                BaseCall =
                    match scall with
                    | Some args -> args
                    | None -> List.empty
                    |> SuperCall
                Body = body }
            |> MemberDef.defaultAccess
            |> Some
            |> def cmembers
        | (None, _, _) ->
            def cmembers None
do
    let members =
        [
            propDef
                (AProperty >> Abstract >> Member)
                (Property >> Concrete >> Member |> Some)
        ]
        |> Seq.map (fun def -> def ClassMember)
    classBodyRef :=
        memberBlock
            members
            [ classDef Type ClassMember ]
        <?> "class body"

let private moduleBody, private moduleBodyRef = createParserForwardedToRef<_, _>()
let moduleDef modl mdef modfs =
    let moduleName =
        simpleName
        >>= fun name ->
            { Body = List.empty
              ModuleName = name
              Members = MemberSet.moduleSet }
            |> modl
            |> mdef
            |> tryAddPlaceholder
            >>% name
    keyword "module"
    >>. noModifiers modfs
    >>. tuple2
        moduleName
        moduleBody
    <?> "module definition"
    |>> fun (name, (body, members)) ->
        { Body = body
          ModuleName = name
          Members = members }
        |> modl
do
    let functionDef modfs =
        let funcHeader =
            genericName
            .>>. paramTupleList typeAnnOpt
            .>> space
            >>= fun (name, fparams) ->
                { Function =
                    { Body = List.empty
                      Parameters = fparams
                      ReturnType = None }
                  FunctionName = name }
                |> Function
                |> Member
                |> ModuleMember
                |> tryAddPlaceholder
                >>% (name, fparams)
        noModifiers modfs
        .>> updateUserState newParams
        >>. tuple3
            funcHeader
            (typeAnnOpt .>> space)
            functionBody
        .>> (tryUpdateState popParams)
        |>> fun ((name, fparams), retType, body) ->
            { Function =
                { Body = body
                  Parameters = fparams
                  ReturnType = retType }
              FunctionName = name }
            |> Function
            |> Member
    let operatorDef modfs =
        fail "bad"
    let types =
        [
            //classDef (Class >> Type) >> Type
            interfaceDef (Interface >> Type)
            moduleDef (Module >> Type)
        ]
        |> Seq.map (fun def -> def ModuleMember)
    moduleBodyRef :=
        memberBlock
            [
                functionDef
                operatorDef
            ]
            types
        |> memberSection
        <?> "module body"

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
                .>>. lambdaBlock
                |> attempt
                .>> (tryUpdateState popParams)
                <?> "anonymous function"
                |>> fun (parameters, body) ->
                    { Body = body
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

                identifier
                .>>. getUserState
                |>> fun (id, state) ->
                    match getSelfId state with
                    | Result.Ok self ->
                        match id.Generics with
                        | [] when id.Name = self -> SelfRef
                        | _ -> IdentifierRef id
                    | Result.Error _ ->
                        IdentifierRef id

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
            |>> Type
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
                .>> (tryUpdateState popParams)
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
            >>= (replaceMember >> updateUserState)
            |>> ignore

            entrypoint
        ]
        |> choice
        .>> space
        |> many

    space
    >>. tuple4
        (namespaceDecl .>> space)
        (useStatements .>> definitions .>> eof)
        position
        getUserState
    |>> fun (ns, uses, pos, state) ->
        { EntryPoint = state.EntryPoint
          Namespace = ns
          Usings = uses
          Source = pos.StreamName
          Types = invalidOp "How do we get members?"}
