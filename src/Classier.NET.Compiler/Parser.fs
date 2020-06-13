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
let dquotes = skipChar '\"' <?> "quotation mark"
let gtsign = skipChar '>'
let lcurlybracket = skipChar '{' <?> "opening bracket"
let lparen = skipChar '(' <?> "opening parenthesis"
let ltsign = skipChar '<'
let period = skipChar '.' <?> "period"
let rcurlybracket = skipChar '}' <?> "closing bracket"
let rparen = skipChar ')' <?> "closing parenthesis"
let semicolon = skipChar ';' <?> "semicolon"
let lambdaOperator = skipString "=>" |> attempt <?> "lambda operator"

// TODO: These forwarded parsers are not needed.
let nestedTypes, private nestedTypesRef = createParserForwardedToRef<MemberDef,_>()

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

let modifiers: Parser<string list, _> = fail "not implemented"

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

// TODO: Clean up parser and put all the functions in the module.
let compilationUnit: Parser<CompilationUnit, ParserState> =

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
        modifiersOLD
            [
                "inline", Flags.Inline
            ]
        >>. skipString "def"
        >>. space1
        >>. functionHeader f
        .>>. typeAnnotation
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
        modifiersOLD
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

        modifiersOLD
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
