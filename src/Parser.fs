module Classier.NET.Compiler.Parser

open System
open System.Collections.Immutable
open FParsec
open Classier.NET.Compiler.AccessControl
open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.Grammar.Operator
open Classier.NET.Compiler.Identifier
open Classier.NET.Compiler.TypeSystem

type StatementEnd =
    | RequireEnd
    | OptionalEnd

type State =
    { EntryPoint: EntryPoint option
      StatementEnd: StatementEnd }

let defaultState =
    { EntryPoint = None
      StatementEnd = RequireEnd }

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

let accessModifier lowest def modfs =
    let modifiers =
        modfs
        |> Seq.map (fun (str, vis) ->
            if vis <= lowest then
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

            preturn def
        ]
let memberAccess lowest =
    [
        "public", Public
        "internal", Internal
        "protected", Protected
        "private", Private
    ]
    |> accessModifier lowest Public
let globalAccess =
    [
        "public", GlobalPublic
        "internal", GlobalInternal
    ]
    |> accessModifier GlobalInternal GlobalPublic

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
            "infix"
            "inline"
            "mutator"
            "override"
            "prefix"
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
                            |> sprintf "Duplicate modifier '%s'"
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
let private badModfier str = Some str |> Result.Error

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

let statement, private statementRef = createParserForwardedToRef<PStatement,_>()
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

let private expressionRef = OperatorPrecedenceParser<_,_,_>()
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

do
    let simpleType =
        choice
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
    let modifiedType f p prev =
        prev
        .>>. opt p
        |>> fun (btype: TypeName, mtype) ->
            match mtype with
            | Some modf -> f btype modf
            | None -> btype
    let arrayType =
        space
        >>. skipChar '['
        |> attempt
        >>. space
        >>. skipChar ']'
        |> many1
        |> modifiedType
            (fun itype nest ->
                Seq.fold
                    (fun prev () ->
                        ArrayType prev)
                    itype
                    nest)
    let functionType =
        space
        >>. lambdaOperator
        >>. space
        >>. (typeName <?> "return type")
        |> attempt
        |> modifiedType
            (fun ptype rettype ->
                FuncType
                    {| ParamType = ptype
                       ReturnType = rettype |})
    typeNameRef :=
        simpleType
        |> arrayType
        |> functionType
        <?> "type name"

let private pattern =
    [
        identifierStr
        .>> space
        |> attempt
        .>>. typeAnnOpt
        |>> VarPattern
        <?> "variable pattern"

        paramTuple typeAnnOpt
        |>> TuplePattern
        <?> "tuple deconstruction"
    ]
    |> choice

do
    statementRef :=
        position
        .>>. choiceL
            [
                semicolon >>% Empty <?> "empty statement"

                choice
                    [
                        keyword "let" >>% LetDecl
                        keyword "var" >>% VarDecl
                    ]
                |> attempt
                .>>. pattern
                .>> space
                <?> "local variable or function"
                >>= fun (decl, p) ->
                    let value =
                        equalsExpr
                        |> attempt
                        .>> space
                        .>> semicolon
                        <?> "variable value"
                    let rest =
                        match p with
                        | VarPattern (_, vtype) ->
                            match vtype with
                            | None ->
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
                            | Some _ -> None
                        | _ -> None
                    rest
                    |> Option.defaultValue value
                    |>> fun value ->
                        decl (p, value)

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
            
                expression
                .>> space
                .>>. choice
                    [
                        followedBy rcurlybracket
                        >>% Return
                        <?> "implicit return"

                        getUserState
                        >>= (fun state ->
                            match state.StatementEnd with
                            | RequireEnd -> semicolon
                            | OptionalEnd ->
                                optional semicolon
                                >>. setUserState { state with StatementEnd = RequireEnd })
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
let genericName: Parser<GenericName, _> =
    let identifier =
        identifierStr
        |> attempt
        .>> space
        .>>. genericParams
        .>> space
        |>> fun (name, gparams) ->
            { Name = name
              Generics = gparams }
    tuple2
        position
        identifier
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
                     : Signature<unit, TypeName>
                  MethodName = name }
                |> amthd
    <?> "method definition"
    |> attempt
let propDef aprop cprop modfs =
    validateModifiers
        modfs
        (fun (isabst, ispure) modf ->
            match (modf, isabst, ispure) with
            | ("abstract", _, IsMutator)
            | ("mutator", true, _) ->
                badModfier "An 'abstract' method cannot be a 'mutator' since it does not have a body"
            | ("abstract", _, IsPure) -> Result.Ok (true, ispure)
            | ("mutator", _, _) -> Result.Ok (isabst, IsMutator)
            | _ -> Result.Error None)
        (false, IsPure)
    >>= fun (isAbstract, purity) ->
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
                  Purity = purity
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
            |> attempt
            >>. choice memberDefs
            <?> "member"

            types
            |> Seq.map (fun def ->
                def modfs
                .>> space)
            |> choice
        ]
        |> choice
let memberBlock label acc members types =
    memberAccess acc
    .>>. memberDef
        members
        types
    |> attempt
    .>> space
    |> many
    |> block
    <?> label
let memberBody label (members: seq<_>) types =
    blockChoice
        [
            statement
            |> attempt
            |>> Choice1Of2

            memberAccess Access.Private
            .>>. memberDef members types
            |>> Choice2Of2
        ]
    <?> label
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
        memberBlock
            "interface body"
            Access.Internal
            members
            [ interfaceDef Type ]

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
                           { Body = List.empty // TODO: Create a body for the equals method here.
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
        memberAccess Access.Private
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
        memberBody
            "class body"
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

let private moduleBody, private moduleBodyRef = createParserForwardedToRef<_, _>()
let moduleDef mdef modfs =
    keyword "module"
    >>. noModifiers modfs
    >>. tuple2
        (simpleName .>> space)
        moduleBody
    <?> "module definition"
    |>> fun (name, members) ->
        { ModuleName = name
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
        |> attempt
        <?> "function definition"
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
        |> between
            (lparen >>. space)
            (space .>> rparen)
        |> attempt
        <?> "operator symbol"
        |>> OperatorStr
    let operatorDef modfs =
        validateModifiers
            modfs
            (fun opkind modf ->
                match (modf, opkind) with
                | ("infix", Some _)
                | ("prefix", Some _) -> badModfier "An operator can only be either an 'infix' or 'prefix' operator"
                | ("infix", None) -> Some Infix |> Result.Ok
                | ("prefix", None) -> Some Prefix |> Result.Ok
                | _ -> Result.Error None)
            (None)
        >>= function
            | None -> fail "An operator must declare whether it is an 'infix' or a 'prefix' operator"
            | Some opkind ->
                tuple4
                    (opName .>> space)
                    (paramTuple typeAnnOpt .>> space)
                    (typeAnnOpt .>> space)
                    functionBody
                <?> "operator definition"
                |>> fun (name, oper, retType, body) ->
                    { Body = body
                      Kind = opkind
                      Operands = oper
                      ReturnType = retType
                      Symbol = name }
                    |> Operator
                    |> Member
    moduleBodyRef :=
        memberBlock
            "module body"
            Access.Private
            [
                functionDef
                operatorDef
            ]
            [
                classDef (Class >> Type)
                interfaceDef (Interface >> Type)
                moduleDef (Module >> Type)
            ]

let private optionalEnd =
    updateUserState
        (fun state -> { state with StatementEnd = OptionalEnd })

let private ifExpr, private ifExprRef = createParserForwardedToRef<_, _>()
do
    ifExprRef :=
        keyword "if"
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
                        .>>. ifExpr
                        |> attempt
                        |>> fun (pos, e) -> [ pos, Return e ]

                        space
                        >>. statementBlock
                        |> attempt;
                    ]
                <?> "else or else-if"

                preturn []
            ]
        .>> optionalEnd
        <?> "if expression"
        |>> fun ((condition, body), rest) ->
            { Condition = condition
              Choice1 = body
              Choice2 = rest }
            |> IfExpr

let private matchCases =
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

let private tryExpr =
    let catchBlock =
        space
        >>. keyword "catch"
        |> attempt
        >>. matchCases
        <?> "catch block"
        |> optList
    let finallyBlock =
        space
        >>. keyword "finally"
        |> attempt
        >>. statementBlock
        <?> "finally block"
        |> optList
    keyword "try"
    >>. tuple3
        (attempt statementBlock <?> "try block")
        catchBlock
        finallyBlock
    .>> optionalEnd
    <?> "try expression"
    >>= fun (tryBlock, catchBlock, finallyBlock) ->
        if List.isEmpty catchBlock && List.isEmpty finallyBlock then
            fail "Expected at least one catch or finally block"
        else
            { TryBody = tryBlock
              Handlers = catchBlock
              Finally = finallyBlock }
            |> TryExpr
            |> preturn

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

                ifExpr

                keyword "match"
                >>. space
                >>. parenExpr
                |> attempt
                .>> space
                .>>. matchCases
                .>> optionalEnd
                <?> "match expression"
                |>> fun (against, cases) ->
                    { Against = against
                      Cases = cases }
                    |> MatchExpr

                tryExpr

                skipString "null" >>% NullLit <?> "null literal"

                skipString "throw"
                >>. choice
                    [
                        space1
                        |> attempt
                        >>. expression
                        |>> Some

                        preturn None // NOTE: Require semicolon?
                    ]
                |> attempt
                <?> "throw expression"
                |>> ThrowExpr

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

let compilationUnit: Parser<CompilationUnit, State> =
    let namespaceDecl =
        skipString "namespace"
        >>. space1
        |> attempt
        >>. sepBy1 identifierStr (separator period)
        .>> space
        .>> semicolon
        |> optList
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
            let eparam = // TODO: Be less strict with parameters, allow none or more than one.
                between
                    (lparen .>> space)
                    (rparen >>. space)
                    (param typeAnnExp)
                .>> space
            getUserState
            >>= fun state ->
                match state.EntryPoint with
                | Some existing ->
                    existing.Origin
                    |> sprintf "An entry point already exists at %O"
                    |> fail
                | None ->
                    tuple3
                        (position .>> keyword "main" |> attempt)
                        (paramTuple typeAnnExp .>> space)
                        functionBody
                    >>= fun (pos, eargs, body) ->
                        let entrypoint =
                            { Arguments = eargs
                              Body = body
                              Origin = pos }
                            |> Some
                        setUserState { state with EntryPoint = entrypoint }
        [
            globalAccess
            .>>. typeDef
            |>> Some

            entrypoint >>% None
        ]
        |> choice
        .>> space
        |> many
        |>> List.choose id
    space
    >>. tuple4
        (namespaceDecl .>> space)
        (useStatements .>> space)
        (definitions .>> eof)
        position
    |>> fun (ns, uses, types, pos) ->
        { Namespace = Namespace ns
          Usings = uses
          Source = pos.StreamName
          Types = types }

let parseFiles enc paths =
    let parseFile state path =
        runParserOnFile
            compilationUnit
            state
            path
            enc
    fun() ->
        Seq.fold
            (fun acc path ->
                match acc with
                | Result.Ok (list: ImmutableList<_>, state) ->
                    match parseFile state path with
                    | Success (cu, nstate, _) ->
                        Result.Ok (list.Add cu, nstate)
                    | Failure (_, err, _) -> Result.Error err
                | Result.Error _ -> acc)
            (Result.Ok (ImmutableList.Empty, defaultState))
            paths
        |> Result.map (fun (cunits, estate) -> cunits, estate.EntryPoint)
