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
    >>. colon
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
            "extern"
            "implicit"
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
let private noModifiers modfs msg =
    validateModifiers
        modfs
        (fun _ _ -> Result.Error msg)
        ()
let private errInvalidModf modf t =
    modf
    |> sprintf "The modifier %s is not valid on %s" t
    |> Result.Error

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

let param =
    identifierStr
    .>>. typeAnnotation
    <?> "parameter"
    |>> fun (name, ptype) ->
        { Name = name
          Type = ptype }
let paramTuple =
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
                        | VarPattern (name, vtype) -> // TODO: Is name needed here? Maybe simplify things by introducing a Statement case that represents a local functon?
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
    identifierStr
    .>> space
    .>> period
    .>> space
    <?> "self identifier"
    |> attempt
    |> opt

let emptyBody = semicolon >>% List.empty
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

            emptyBody
        ]
        "function body"
let functionDef modfs =
    let funcHeader =
        genericName
        .>>. paramTupleList
        .>> space
        >>= fun (name, fparams) ->
            let placeholder =
                MemberDef.placeholderFunc
                    name
                    fparams
            tryAddMember (Access.Public, placeholder) >>% (name, fparams)
    noModifiers
        modfs
        "Modifiers are not valid on a function"
    >>. tuple3
        funcHeader
        (typeAnnotation .>> space)
        functionBody
    |>> fun ((name, fparams), retType, body) ->
        Function
            {| Function =
                { Body = body
                  Parameters = fparams
                  ReturnType = retType }
               FunctionName = name |}
let methodDef body modfs =
    let methodModf =
        validateModifiers
            modfs
            (fun prev modf ->
                match modf with
                | "abstract" ->
                    let redundantAbstract = Result.Error "An abstract method cannot also be sealed"
                    match prev.ImplKind with
                    | AbstractOrSealed _ -> redundantAbstract
                    | Override nested ->
                        match nested with
                        | None -> Result.Ok { prev with ImplKind = Some MethodInheritance.Abstract |> Override }
                        | Some _ -> redundantAbstract
                    | Virtual -> Result.Error "An abstract method already implies that it can be overriden, making the virtual modifier redundant"
                | "sealed" ->
                    let badInhType = function
                        | MethodInheritance.Abstract -> Result.Error "A sealed method cannot also be abstract"
                        | MethodInheritance.Sealed -> Result.Error "The sealed modifier is redundant, since methods cannot be overriden by default"
                    match prev.ImplKind with
                    | AbstractOrSealed inhType -> badInhType inhType
                    | Override nested ->
                        match nested with
                        | None -> Result.Ok { prev with ImplKind = Some MethodInheritance.Sealed |> Override }
                        | Some _ -> badInhType nested.Value
                    | Virtual -> Result.Error "Virtual methods cannot be sealed"
                | "mutator" -> Result.Ok { prev with IsMutator = true }
                | _ -> errInvalidModf modf "methods")
            MethodModifiers.Default
    let methodHeader =
        tuple3
            selfId
            genericName
            paramTupleList
        |> attempt
        .>> space
        >>= fun (selfId, name, mparams) ->
            let placeholder =
                MemberDef.placeholderMethod
                    name
                    selfId
                    mparams
            tryAddMember (Access.Public, placeholder) >>% (selfId, name, mparams)
    tuple4
        methodModf
        methodHeader
        (typeAnnotation .>> space)
        body
    |>> fun (mmodf, (selfId, name, mparams), retType, body) ->
        Method
            {| Method =
                { Body = body
                  Parameters = mparams
                  ReturnType = retType }
               MethodName = name
               Modifiers = mmodf
               SelfIdentifier = selfId |}

let propDef body modfs = // TODO: Use Statement list option for methodDef, propDef, functionDef, etc. to represent no body for abstract methods and auto properties.
    let propName =
        genericName
        >>= fun name ->
            (Access.Public, MemberDef.placeholderProp name)
            |> tryAddMember
            >>% name
    let propBody =
        let setFull =
            keyword "set"
            >>. lparen
            >>. space
            >>. param
            .>> space
            .>> rparen
            .>> space
            .>>. body
            |> opt
        let setEmpty =
            keyword "set"
            .>> semicolon
            >>% None
        [
            keyword "get"
            >>. choice
                [
                    emptyBody
                    .>> space
                    .>>. setEmpty

                    body
                    .>> space
                    .>>. setFull
                ]
        ]
        |> choice
        |> block
    let propValue =
        space
        >>. equalsExpr
        |> attempt
        |> opt
    noModifiers
        modfs
        "Modifiers are not valid on a property"
    >>. tuple5
        selfId
        propName
        (typeAnnotation .>> space)
        propBody
        propValue
    <?> "property definition"
    |>> fun (selfid, name, ptype, (get, set), pvalue) ->
        Property
            {| Accessors =
                 match get with
                 | [] ->
                     match set with
                     | None -> PropertyAccessors.AutoGet
                     | Some _ -> PropertyAccessors.AutoGetSet
                 | _ ->
                     match set with
                     | None -> PropertyAccessors.Get get
                     | Some (setParam, setBody) -> PropertyAccessors.GetSet(get, setParam, setBody)
               PropName = name
               SelfIdentifier = selfid
               Value = pvalue
               ValueType = ptype |}

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
                |>> Type
                .>> space)
            |> choice
        ]
        |> choice
let memberBlock members types =
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
    >>. noModifiers
        modfs
        "Modifiers are not valid on an interface"
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
    interfaceMembersRef :=
        accessModifier Access.Internal
        >>. memberDef
            [
                methodDef emptyBody // TODO: Instead of empty body, use (fail "message"), since the methodDef and propDef parsers should be checking for empty bodies.
                propDef emptyBody
            ]
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
                    Name.OfString str pos
                    |> Option.get
                let selfid = "__this"
                seq {
                    Method
                        {| Method =
                            { Body = List.empty // TODO: Create a body here.
                              Parameters = [ [ Param.OfName "obj" TypeName.Inferred ] ]
                              ReturnType = TypeName.Primitive PrimitiveType.Boolean }
                           MethodName = name "equals" pos
                           Modifiers = MethodModifiers.Default
                           SelfIdentifier = Some selfid |}

                    yield!
                        Seq.map
                            (fun (param: Param) ->
                                Property
                                    {| Accessors = AutoGet
                                       PropName = name param.Name pos
                                       SelfIdentifier = None
                                       Value = None
                                       ValueType = param.Type |})
                            values
                }
                |> Seq.map (fun mdef -> Access.Public, mdef)
        |> opt
    let classModf =
        validateModifiers
            modfs
            (fun inheritKind modf ->
                match (inheritKind, modf) with
                | (MustInherit, "inheritable")
                | (CanInherit, "abstract") -> Result.Error "An abstract class already implies that it is inheritable"
                | (_, "inheritable") -> Result.Ok CanInherit
                | (_, "abstract") -> Result.Ok MustInherit
                | _ -> errInvalidModf modf "classes")
            (Sealed)
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
    let def header label body =
        tuple8
            header
            classModf
            (genericTypeName TypeDef.placeholderClass)
            classCtor
            classBase
            (implements .>> space)
            classSelfId
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
        let (ctorAcc, primaryCtor) =
            superCall
            |> SuperCall
            |> ctor body
        Class
            {| ClassName = name
               Body = body
               Inheritance = cmodf
               Interfaces = ilist
               Members =
                 match rmembers with
                 | Some recordMems ->
                    primaryCtor.Parameters
                    |> recordMems
                    |> cmembers.Union
                 | None -> cmembers
               PrimaryCtor = ctorAcc, primaryCtor
               SelfIdentifier = selfid
               SuperClass = sclass |}
do
    classBodyRef :=
        memberBlock
            [
                methodDef functionBody
                propDef functionBody
            ]
            [
                classDef
                interfaceDef
            ]
        <?> "class body"

let private moduleBody, private moduleBodyRef = createParserForwardedToRef<_, _>()
let moduleDef modfs =
    keyword "module"
    >>. noModifiers
        modfs
        "Modifiers are not valid on aa module"
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
            tuple3
                mainDef
                (paramTuple .>> space)
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

    (newMembers >> pushValidator typeValidator)
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
