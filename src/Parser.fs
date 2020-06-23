﻿module Classier.NET.Compiler.Parser

open System
open System.Collections.Immutable
open FParsec
open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.GlobalType
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.Identifier
open Classier.NET.Compiler.TypeSystem

let private position: Parser<_, _> = fun stream -> Reply(stream.Position)

let private optList p = opt p |>> Option.defaultValue []

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

let typeName, private typeNameRef = createParserForwardedToRef<TypeName, _>()
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
let private badModfier str = Some str  |> Result.Error // TODO: Fix the name. Or maybe this function isn't necessary at all?

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

let extends = keyword "extends" >>. identifierFull
let implementsOpt =
    keyword "implements"
    >>. sepBy1
        identifierFull
        (separator comma)
    |> optList
    <?> "interface implementations"

let param typeAnn =
    identifierStr
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
        .>>. opt extends
        .>>. implementsOpt
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

let selfId =
    identifierStr
    .>> space
    .>> period
    .>> space
    <?> "self identifier"
    |> attempt
    |> opt
let selfIdAs =
    keyword "as"
    |> attempt
    >>. identifierStr
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

let methodDef amthd cmthd modfs =
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
    >>= fun (mmodf, isAbstract) ->
        match cmthd with
        | Some cmthd when (not isAbstract) ->
            tuple5
                selfId
                genericName
                (paramTupleList typeAnnOpt .>> space)
                (typeAnnOpt .>> space)
                functionBody
            |>> fun (selfId, name, mparams, retType, body) ->
                { Method =
                    { Body = body
                      Parameters = mparams
                      ReturnType = retType }
                  MethodName = name
                  Modifiers = mmodf
                  SelfIdentifier = selfId }
                |> cmthd
        | _ ->
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
                |> amthd
    <?> "method definition"
    |> attempt // TODO: Can the attempt be moved elsewhere? Check both methodDef and propDef
let propDef aprop cprop modfs =
    validateModifiers
        modfs
        (fun _ modf ->
            match modf with
            | "abstract" -> Result.Ok true
            | _ -> Result.Error None)
        false
    >>= fun isAbstract ->
        match cprop with
        | Some cprop when (not isAbstract) ->
            let propBody =
                let setFull =
                    keyword "set"
                    |> attempt
                    >>. lparen
                    >>. space
                    >>. param typeAnnOpt
                    .>> space
                    .>> rparen
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
                simpleName
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
            let propBody =
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
                simpleName
                (typeAnnExp .>> space)
                propBody
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
            >>. choice memberDefs // TODO: Are the placeholder members ever replaced here?
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
            |>> Choice1Of2

            accessModifier Access.Private
            .>>. memberDef members types
            |>> Choice2Of2
        ]
    |>> List.fold
            (fun (body: ImmutableList<_>, members: ImmutableList<_>) item ->
                match item with
                | Choice1Of2 st -> body.Add st, members
                | Choice2Of2 mdef -> body, members.Add mdef)
            (ImmutableList.Empty, ImmutableList.Empty)

let private interfaceBody, private interfaceBodyRef = createParserForwardedToRef<_, _>()
let interfaceDef idef modfs =
    keyword "interface"
    >>. noModifiers modfs
    >>. tuple3
        genericName
        (implementsOpt .>> space)
        interfaceBody
    <?> "interface definition"
    |>> fun (name, ilist, members) ->
        { InterfaceName = name
          Members = members
          SuperInterfaces = ilist }
        |> idef
do
    let members =
        [
            methodDef
                (AMethod >> Member)
                None

            propDef
                (AProperty >> Member)
                None
        ]
        |> Seq.map
            (fun def ->
                fun (rest: ImmutableList<_>) ->
                    "abstract"
                    |> rest.Add
                    |> def)
    interfaceBodyRef :=
        accessModifier Access.Internal
        .>>. memberDef
            members
            [ interfaceDef Type ]
        |> attempt
        .>> space
        |> many
        |> block
        <?> "interface body"

let private classBody, private classBodyRef = createParserForwardedToRef<_ * ImmutableList<_ * ClassMember>, _>()
let classDef cdef modfs =
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
                                       |> Param.create None
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
                |> Seq.map (fun mdef -> Access.Public, Concrete mdef |> ClassMember.Member)
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
        .>>. (paramTuple typeAnnOpt |> optList)
        .>> space
        <?> "primary constructor"
    let classBase =
        extends
        .>> space
        .>>. optList tupleExpr
        .>> space
        |> opt
        |>> fun cbase ->
            match cbase with
            | Some (basec, baseargs) -> Some basec, baseargs
            | None -> None, List.empty
    let def header label body =
        tuple8
            header
            classModf
            genericName
            classCtor
            selfIdAs
            classBase
            (implementsOpt .>> space)
            body
        <?> label
    [
        [
            semicolon >>% (ImmutableList.Empty, ImmutableList.Empty)
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
    |>> fun (rmembers, cmodf, name, (ctoracc, ctorparams), selfid, (basec, baseargs), ilist, (body, cmembers)) ->
        { ClassName = name
          Body = List.ofSeq body
          Inheritance = cmodf
          Interfaces = ilist
          Members =
            match rmembers with
            | Some toadd -> cmembers.InsertRange(0, toadd ctorparams)
            | None -> cmembers
          PrimaryCtor = (ctoracc, ctorparams, baseargs)
          SelfIdentifier = selfid
          SuperClass = basec }
        |> cdef
do
    let ctorDef modfs =
        let ctorCall =
            lambdaOperator
            .>> space
            >>. expression
            .>> space
            .>> semicolon
            <?> "primary call"
        keyword "new"
        |> attempt
        >>. noModifiers modfs
        >>. tuple3
            (paramTuple typeAnnOpt .>> space)
            selfIdAs
            ctorCall
        <?> "constructor definition"
        |>> fun (cparams, selfid, call) ->
            { Call = call
              Parameters = cparams
              SelfIdentifier = selfid }
            |> Constructor
            |> Concrete
            |> Member
    classBodyRef :=
        memberBlock
            [
                ctorDef

                methodDef
                    (AMethod >> Abstract >> Member)
                    (Method >> Concrete >> Member |> Some)

                propDef
                    (AProperty >> Abstract >> Member)
                    (Property >> Concrete >> Member |> Some)
            ]
            [ classDef Type ]
        <?> "class body"

let private moduleBody, private moduleBodyRef = createParserForwardedToRef<ImmutableList<_> * ImmutableList<_>, _>()
let moduleDef mdef modfs =
    keyword "module"
    >>. noModifiers modfs
    >>. tuple2
        (simpleName .>> space)
        moduleBody
    <?> "module definition"
    |>> fun (name, (body, members)) ->
        { Body = List.ofSeq body
          ModuleName = name
          Members = members }
        |> mdef
do
    let functionDef modfs =
        noModifiers modfs
        >>. tuple4
            genericName
            (paramTupleList typeAnnOpt .>> space)
            (typeAnnOpt .>> space)
            functionBody
        |>> fun (name, fparams, retType, body) ->
            { Function =
                { Body = body
                  Parameters = fparams
                  ReturnType = retType }
              FunctionName = name }
            |> Function
            |> Member
    let opName =
        Operator.operatorChars
        |> anyOf
        |> many1Chars
    let operatorDef modfs =
        fail "bad"
    moduleBodyRef :=
        memberBlock
            [
                functionDef
                operatorDef
            ]
            [
                classDef (Class >> Type)
                interfaceDef (Interface >> Type)
                moduleDef (Module >> Type)
            ]
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

                paramTuple typeAnnOpt
                .>> space
                .>>. lambdaBlock
                |> attempt
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

let compilationUnit: Parser<CompilationUnit, EntryPoint option> =
    let namespaceDecl =
        skipString "namespace"
        >>. space1
        |> attempt
        >>. sepBy1 identifier (separator period)
        |>> FullIdentifier
        .>> space
        .>> semicolon
        |> opt
        <?> "namespace declaration"
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
            >>= fun modfs ->
                [
                    classDef Class
                    interfaceDef Interface
                    moduleDef Module
                ]
                |> Seq.map (fun def -> def modfs)
                |> choice
        let entrypoint =
            getUserState
            >>= function
                | Some existing ->
                    string existing.Origin
                    |> sprintf "An entry point already exists at %s"
                    |> fail
                | None ->
                    position
                    .>> keyword "main"
                    |> attempt
                    .>>. paramTuple typeAnnExp
                    .>> space
                    .>>. functionBody
                    >>= fun ((pos, eparams), body) ->
                        { Body = body
                          Origin = pos
                          Parameters = eparams }
                        |> Some
                        |> setUserState
        [
            Access.Internal
            |> accessModifier
            .>>. typeDef
            |>> Some

            entrypoint >>% None
        ]
        |> choice
        .>> space
        |> many
        |>> List.choose id
    space
    >>. tuple5
        (namespaceDecl .>> space)
        (useStatements .>> space)
        (definitions .>> eof)
        position
        getUserState
    |>> fun (ns, uses, types, pos, entryPoint) ->
        { EntryPoint = entryPoint
          Namespace = ns
          Usings = uses
          Source = pos.StreamName
          Types = types }
