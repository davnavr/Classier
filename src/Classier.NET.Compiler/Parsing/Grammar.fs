// Copyright (c) 2020 David Navarro
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

module Classier.NET.Compiler.Parsing.Grammar

open System
open FParsec
open Classier.NET.Compiler

[<Flags>]
type Flags =
    | None = 0uy
    | Public = 0uy
    | Internal = 1uy
    | Protected = 2uy
    | Private = 3uy
    | Inline = 4uy
    | Abstract = 8uy
    /// Indicates that a class, field, or local variable is mutable,
    /// or that a method changes the value of a field.
    | Mutable = 16uy
    /// Indicates that a class can have a subclass.
    | Inheritable = 32uy
    | VisibilityMask = 3uy
    | ModifierMask = 252uy

[<Flags>]
type NumType =
    | Decimal = 0uy
    | Double = 1uy
    | Float = 2uy
    | Signed = 0uy
    | Unsigned = 4uy
    | Integer = 8uy
    | Long = 16uy

type NumLiteral =
    { Base: byte
      FracPart: char list
      IntPart: char list
      Type: NumType }

type Definition =
    { Flags: Flags
      Name: string }

type TypeName =
    | Identifier of Identifier list
    | Inferred
    | Tuple of TypeName list
    | Union of TypeName list

    override this.ToString() =
        match this with
        | Identifier names -> String.Join('.', names)
        | Inferred -> "_"
        | Tuple types ->
            String.Join(", ", types)
            |> sprintf "(%s)"
        | Union options -> String.Join(" | ", options)
and Identifier =
    { Name: string
      GenericArgs: GenericArg list }

    override this.ToString() =
        match this.GenericArgs with
        | [] -> this.Name
        | _ ->
            let gargs = String.Join(", ", this.GenericArgs)
            sprintf "%s<%s>" this.Name gargs
and GenericArg = TypeName

type Expression =
    | CtorCall of
        {| Arguments: Expression list
           Type: TypeName |}
    | FuncCall of
        {| Arguments: Expression list list
           Target: Expression |}
    | IdentifierRef of Identifier
    | IfExpr of If
    | MatchExpr of Match
    | MemberAccess of Expression * Identifier
    | Nested of Expression
    | NumLit of NumLiteral
    | TupleLit of Expression list
    | UnitLit
    | ValAssignment of Assignment
    | ValDeclaration of Assignment
and Assignment =
    { Target: Expression
      Value: Expression }
and If =
    { Condition: Expression
      Choice1: Statement list
      Choice2: Statement list }
and Match =
    { Against: Expression
      Cases: MatchCase list }
and MatchCase =
    { Body: Statement list
      Patterns: MatchPattern list }
and MatchPattern =
    | Constant of Expression
    | CasePattern of
        {| CaseName: Identifier list
           Values: string option list |}
    | Default
and Statement =
    | Empty
    | IfStatement of If
    /// An expression whose result is evaluated then discarded.
    | IgnoredExpr of Expression
    | LocalVar of Variable
    | MatchStatement of Match
    | Return of Expression
    | While of Expression * Statement list
and Variable =
    { VarDef: Definition
      Type: TypeName
      Value: Expression option }

type Param =
    { Name: string
      Type: TypeName }

type GenericVariance =
    | NoVariance
    | Covariant
    | Contravariant

type GenericParam =
    { Name: string
      RequiredSuperClass: Identifier list
      RequiredInterfaces: TypeName list
      Variance: GenericVariance }

type Function =
    { Body: Statement list
      FuncDef: Definition
      GenericParams: GenericParam list
      Parameters: Param list list
      ReturnType: TypeName }

type CtorBaseCall =
    | NoBaseCall
    | SelfCall of Expression list
    | SuperCall of Expression list

type TypeHeader =
    | Class of Identifier list
    | DUnion
    | Interface
    | Module
    | Record
and TypeDef =
    { Definition: Definition
      GenericParams: GenericParam list
      Header: TypeHeader
      Interfaces: TypeName list
      Members: MemberDef list }
and MemberDef =
    | Ctor of
        {| BaseCall: CtorBaseCall
           Body: Statement list
           CtorDef: Definition
           Parameters: Param list |}
    | Field of Variable
    | Function of Function
    | Property of 
        {| Get: Function option
           PropDef: Definition
           Set: Function option
           Value: Expression option |}
    | NestedType of TypeDef
    | DUnionCase of string * TypeName option

type CompilationUnit =
    { TypeDefs: TypeDef list
      Namespace: string list
      Usings: Identifier list list }

type ParserState =
    { CurrentFlags: Flags
      Symbols: SymbolTable }

    static member Default =
        { CurrentFlags = Flags.None
          Symbols = SymbolTable.empty }

    member this.VisibilityFlags = this.CurrentFlags &&& Flags.VisibilityMask

    member this.CreateDefinition name =
        { Flags = this.CurrentFlags
          Name = name }

let parser: Parser<CompilationUnit, ParserState> =
    let clearModifierFlags =
        updateUserState (fun state ->
            { state with CurrentFlags = state.VisibilityFlags })
    let setFlags flags =
        (fun state ->
            let vis = flags &&& Flags.VisibilityMask
            let mdf = flags &&& Flags.ModifierMask
            { state with CurrentFlags = state.CurrentFlags ||| vis ||| mdf })
        |> updateUserState
    let updateSymbolTable f p =
        p
        .>>. getUserState
        >>= fun (data, state) ->
            let newState = { state with Symbols = f data state.Symbols }
            setUserState newState >>. preturn data

    let colon = skipChar ':'
    let comma = skipChar ','
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

    let accessModifier lowest =
        let modifiers =
            [
                "public", Flags.Public
                "internal", Flags.Internal
                "protected", Flags.Protected
                "private", Flags.Private
            ]
            |> Seq.filter (fun (_, vis) -> vis <= lowest)
            |> Seq.map (fun (str, vis) ->
                skipString str
                |> attempt
                .>> setFlags vis)
            |> choice
        modifiers
        .>> clearModifierFlags
        <?> "access modifier"

    let classDef, classDefRef = createParserForwardedToRef<TypeDef,_>()
    let identifier, identifierRef = createParserForwardedToRef<Identifier,_>()
    let ifStatement, ifStatementRef = createParserForwardedToRef<If,_>()
    let matchStatement, matchStatementRef = createParserForwardedToRef<Match, _>()
    let tuple, tupleRef = createParserForwardedToRef<Expression list,_>()
    let typeName, typeNameRef = createParserForwardedToRef<TypeName,_>()
    let statementBlock, statementBlockRef = createParserForwardedToRef<Statement list,_>()

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
        >>. setFlags flag
        |> optional

    let separator p = ignored >>. p .>> ignored

    let block p =
        lcurlybracket
        |> attempt
        >>. ignored
        >>. p
        .>> ignored
        .>> rcurlybracket
    let blockChoice p =
        choice p
        .>> ignored
        |> many
        |> block

    let genericArgs =
        ltsign
        >>. sepBy1 typeName (separator comma)
        |> attempt
        .>> gtsign
        |> opt
        |>> Option.defaultValue []
        <?> "generic arguments"

    let identifierStr =
        asciiLetter
        .>>. manyChars (asciiLetter <|> digit)
        |>> String.Concat
        <?> "identifier"
    let identifierList =
        sepBy1 identifier (separator period)
    let identifierFull = identifierList |>> Identifier

    let paramTuple =
        lparen
        .>> ignored
        >>. sepBy
                (identifierStr
                .>>. typeAnnotationOpt
                |>> fun (name, ptype) ->
                    { Name = name
                      Type = ptype })
                (separator comma)
        .>> rparen
        <?> "parameters"

    let expression =
        let decimalChars = [ '0'..'9' ]
        let expr = OperatorPrecedenceParser<_,_,_>()
        let exprParser = expr.ExpressionParser <?> "expression"
        let functionCall targetExpr args name =
            let target = MemberAccess (targetExpr, { Name = name; GenericArgs = List.empty })
            FuncCall {| Arguments = args; Target = target |}
        let assignment t target value = t { Target = target; Value = value }

        [
            "equals", "==", 30, Associativity.Left

            // Mathematical operators
            "add", "+", 40, Associativity.Left
            "subtract", "-", 40, Associativity.Left
            "multiply", "*", 50, Associativity.Left
            "divide", "/", 50, Associativity.Left
        ]
        |> Seq.map (fun (name, op, prec, assoc) ->
            let map expr1 expr2 =
                functionCall expr1 [ [ expr2 ] ] name
            InfixOperator<_,_,_>(op, ignored, prec, assoc, map))
        |> Seq.cast<Operator<_,_,_>>
        |> Seq.append
            [
                InfixOperator<_,_,_>("|>", ignored, 20, Associativity.Left, fun args f -> FuncCall {| Arguments = [ [ args ] ]; Target = f |});
                PrefixOperator<_,_,_>("-", ignored, 60, true, fun exp -> functionCall exp [] "negate");
                InfixOperator<_,_,_>("<-", ignored, 100, Associativity.Left, assignment ValAssignment);
                InfixOperator<_,_,_>("=", ignored, 100, Associativity.Left, assignment ValDeclaration);
            ]
        |> Seq.iter expr.AddOperator

        expr.TermParser <-
            choice
                [
                    ifStatement |>> IfExpr <?> "if expression";

                    matchStatement |>> MatchExpr <?> "match expression";

                    identifier |>> IdentifierRef;

                    tuple
                    <?> "tuple"
                    |>> (fun ex ->
                        match ex with
                        | [] -> UnitLit
                        | [ nested ] -> nested
                        | _ -> TupleLit ex);

                    skipString "new"
                    >>. ignored1
                    |> attempt
                    >>. identifierFull
                    .>> ignored
                    .>>. (tuple <?> "constructor arguments")
                    |>> (fun (ctype, args) -> CtorCall {| Arguments = args; Type = ctype |});

                    pchar '0'
                    >>. choice
                        [
                            anyOf [ 'b'; 'B' ] >>. preturn [ '0'; '1'; ];
                            anyOf [ 'x'; 'X' ] >>. preturn (decimalChars @ [ 'a'..'z' ] @ [ 'A'..'Z' ]);
                        ]
                    |> attempt
                    <|> preturn decimalChars
                    <|> (pchar '.' >>. preturn [])
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
                                |> Seq.map (fun (s, f) -> pstringCI s >>. preturn f)
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
                    tuple
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
                <|> preturn []
                |>> Seq.fold
                    (fun prev next -> next prev)
                    target
        exprParser
    let expressionInParens =
        lparen
        |> attempt
        >>. ignored
        >>. expression
        .>> rparen

    let variableDef =
        clearModifierFlags
        >>. choice
            [
                skipString "let"
                skipString "var" >>. setFlags Flags.Mutable
            ]
        .>> ignored1
        |> attempt
        .>> setFlags Flags.Private
        >>. identifierStr
        .>>. typeAnnotationOpt
        .>> ignored
        .>>. opt
            (opequal
            |> attempt
            >>. ignored
            >>. expression)
        .>> semicolon
        .>>. getUserState
        |>> fun (((name, tann), expr), state) ->
            { VarDef =
                { Name = name
                  Flags = state.CurrentFlags }
              Value = expr
              Type = tann }

    let lambdaBody =
        lambdaOperator
        |> attempt
        >>. ignored
        >>. expression
        |>> Return
        |>> List.singleton
        .>> ignored
        .>> semicolon

    tupleRef :=
        lparen
        |> attempt
        >>. ignored
        >>. sepBy expression (separator comma)
        .>> rparen

    typeNameRef :=
        choice
            [
                sepBy1
                    identifierFull
                    (pchar '|' |> separator)
                |>> fun tlist ->
                    match tlist with
                    | [ one ] -> one
                    | _ -> Union tlist

                skipChar '_'
                >>. preturn Inferred;

                between
                    lparen
                    rparen
                    (sepBy typeName (separator comma))
                |>> Tuple
                <?> "tuple";
            ]
        <?> "type name"

    identifierRef :=
        identifierStr
        .>> ignored
        .>>. genericArgs
        |>> fun (name, gparams) ->
            { Name = name
              GenericArgs = gparams }

    ifStatementRef :=
        skipString "if"
        >>. ignored
        >>. expressionInParens
        .>> ignored
        .>>. statementBlock
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
                        >>. statementBlock;
                    ]

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
        >>= fun against ->
            let pattern =
                choice
                    [

                        identifierList
                        .>> ignored
                        .>> lparen
                        .>> ignored
                        .>>. sepBy1
                            (choice [ underscore >>. preturn None; identifierStr |>> Some ])
                            (separator comma)
                        .>> rparen
                        <??> "case pattern"
                        |>> fun (name, values) ->
                            CasePattern
                                {| CaseName = name
                                   Values = values |}

                        expression |>> Constant

                        underscore
                        >>. ignored
                        >>. preturn Default
                    ]

            sepBy1 pattern (separator comma)
            <?> "pattern"
            .>> ignored
            .>> colon
            |> attempt
            .>> ignored
            .>>. expression
            .>> ignored
            .>> semicolon
            |>> (fun (patterns, expr) -> { Body = [ Return expr ]; Patterns = patterns })
            .>> ignored
            |> many1
            <?> "cases"
            |> block
            |>> fun cases -> { Against = against; Cases = cases }

    statementBlockRef :=
        blockChoice
            [
                semicolon
                >>. preturn Empty
                <?> "empty statement";

                variableDef
                |>> LocalVar
                <?> "local variable";

                skipString "while"
                >>. ignored
                |> attempt
                >>. lparen
                >>. expression
                .>> rparen
                .>> ignored
                .>>. statementBlock
                |>> While
                <?> "while loop";

                skipString "return"
                >>. ignored1
                |> attempt
                >>. expression
                |>> Return
                <?> "return statement"
                .>> ignored
                .>> semicolon;

                ifStatement |>> IfStatement <?> "if statement";

                matchStatement |>> MatchStatement <?> "match statement";
                
                expression
                .>>. choice
                    [
                        ignored
                        >>. optional semicolon
                        >>. ignored
                        >>. followedBy rcurlybracket
                        |> attempt
                        >>. preturn Return
                        <?> "implicit return"

                        ignored
                        >>. semicolon
                        |> attempt
                        >>. preturn IgnoredExpr
                    ]
                |>> fun (expr, statement) -> statement expr;
            ]

    let extends =
        ignored1
        .>> skipString "extends"
        .>> ignored1
        |> attempt
        >>. identifierList
        |> opt
        |>> Option.defaultValue []

    let implements =
        ignored1
        .>> skipString "implements"
        .>> ignored1
        |> attempt
        >>. sepBy1 identifierFull (separator comma)
        |> opt
        |>> Option.defaultValue []
        <?> "interface implementations"

    let genericParams =
        let genericParam =
            choice
                [
                    skipString "in"
                    >>. ignored1
                    |> attempt
                    >>. preturn Contravariant;

                    skipString "out"
                    >>. ignored1
                    |> attempt
                    >>. preturn Covariant;

                    preturn NoVariance;
                ]
            .>>. identifierStr
            |> attempt
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
        |> opt
        |>> Option.defaultValue []
        <?> "generic parameters"

    let functionBody =
        [
            statementBlock
            lambdaBody
            semicolon >>. preturn []
        ]
        |> choice
        <?> "function body"

    let fieldDef =
        variableDef
        |>> Field
        <?> "field definition"

    let ctorDef =
        getUserState
        .>> skipString "new"
        .>> ignored1
        |> attempt
        .>>. paramTuple
        .>> ignored
        .>>.
            (colon
            |> attempt
            >>. ignored
            >>. choice
                [
                    skipString "this" >>. preturn SelfCall
                    skipString "super" >>. preturn SuperCall
                ]
            .>> ignored
            .>>. tuple
            |> opt
            <?> "base call"
            |>> fun (baseCall) ->
                match baseCall with
                | Some (callType, args) -> callType args
                | _ -> NoBaseCall)
        .>> ignored
        .>>. functionBody
        <?> "constructor definition"
        |>> fun (((state, parameters), baseCall), body) ->
            Ctor
                {| BaseCall = baseCall
                   Body = body
                   CtorDef = state.CreateDefinition(String.Empty)
                   Parameters = parameters |}

    let functionDef =
        modifier "inline" Flags.Inline
        >>. modifier "mutator" Flags.Mutable
        >>. getUserState
        .>>. identifierStr
        .>> ignored
        .>>. genericParams
        .>> ignored
        .>>. (paramTuple .>> ignored |> many1 <?> "parameter list")
        |> attempt
        .>>. typeAnnotationOpt
        .>> ignored
        .>>. functionBody
        |>> fun (((((st, name), gparams), fparams), retType), body) ->
            Function
                { Body = body
                  FuncDef =
                    { Name = name
                      Flags = st.CurrentFlags }
                  GenericParams = gparams
                  Parameters = fparams
                  ReturnType = retType }

    let methodDef =
        modifier "abstract" Flags.Abstract
        >>. functionDef
        <?> "method definition"

    let typeDef word =
        skipString word
        >>. ignored1
        |> attempt
        >>. getUserState
        |>> fun state -> state.CreateDefinition

    let recordDef =
        typeDef "data"
        .>>. identifierStr
        .>>. genericParams
        .>> ignored
        .>>. blockChoice
            [
                identifierStr
                .>>. typeAnnotation
                .>> ignored
                .>> semicolon
                <?> "record field"
                |>> fun (name, ftype) ->
                    Field
                        { VarDef =
                            { Name = name
                              Flags = Flags.Public }
                          Value = None
                          Type = ftype }

                accessModifier Flags.Private
                >>. ignored1
                >>. methodDef;
            ]
        <?> "record definition"
        |>> fun (((def, name), gparams), members) ->
            { Definition = def name
              GenericParams = gparams
              Header = Record
              Interfaces = []
              Members = members }

    let unionDef =
        typeDef "union"
        .>>. identifierStr
        .>>. genericParams
        .>> ignored
        .>>. blockChoice
            [
                identifierStr
                .>>. typeAnnotationOpt
                .>> ignored
                .>> semicolon
                <?> "union case"
                |>> fun (name, ctype) ->
                    match ctype with
                    | Inferred -> DUnionCase (name, None)
                    | _ -> DUnionCase (name, Some ctype)
            ]
        <?> "discriminated union"
        |>> fun (((def, name), gparams), cases) ->
            { Definition = def name
              GenericParams = gparams
              Header = DUnion
              Interfaces = []
              Members = cases }

    let interfaceDef =
        typeDef "interface"
        .>>. identifierStr
        .>>. genericParams
        .>>. implements
        .>> ignored
        .>>. blockChoice
            [
                accessModifier Flags.Internal
                >>. ignored1
                >>. choice
                    [
                        methodDef
                    ]
            ]
        |>> (fun ((((def, name), gparams), iimpls), members) ->
            { Definition = def name
              GenericParams = gparams
              Header = Interface
              Interfaces = iimpls
              Members = members })
        <?> "interface definition"

    classDefRef :=
        modifier "abstract" Flags.Abstract
        >>. modifier "inheritable" Flags.Inheritable
        >>. modifier "mutable" Flags.Mutable
        >>. typeDef "class"
        |> attempt
        .>>. identifierStr
        .>>. genericParams
        .>>. extends
        .>>. implements
        .>> ignored
        .>>. blockChoice
            [
                fieldDef

                accessModifier Flags.Private
                >>. ignored1
                >>. choice
                    [
                        ctorDef
                        methodDef
                        classDef |>> NestedType
                    ]
            ]
        <?> "class definition"
        |>> fun (((((def, name), gparams), superclass), iimpls), members) ->
            { Definition = def name
              GenericParams = gparams
              Header = Class superclass
              Interfaces = iimpls
              Members = members }

    let moduleDef =
        typeDef "module"
        .>>. identifierStr
        .>>. genericParams
        .>> ignored
        .>>. blockChoice
            [
                fieldDef;
                
                accessModifier Flags.Private
                >>. ignored1
                >>. choice
                    [
                        recordDef |>> NestedType
                        unionDef |>> NestedType
                        interfaceDef |>> NestedType
                        classDef |>> NestedType
                        functionDef <?> "function definition"
                    ]
            ]
        <?> "module definition"
        |>> fun (((def, name), gparams), members) ->
            { Definition = def name
              GenericParams = gparams
              Header = Module
              Interfaces = []
              Members = members }

    ignored
    >>. opt
        (skipString "namespace"
        >>. ignored1
        |> attempt
        >>. sepBy1 identifierStr (separator period)
        |> updateSymbolTable (SymbolTable.addNamespace)
        .>> ignored
        .>> semicolon
        <?> "namespace declaration")
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
    .>>. (accessModifier Flags.Internal
         >>. ignored1
         >>. choice
             [
                 classDef
                 interfaceDef
                 moduleDef
                 recordDef
                 unionDef
             ]
         .>> ignored
         |> many1)
    .>> eof
    |>> fun ((ns, uses), defs) ->
        { TypeDefs = defs
          Namespace = Option.defaultValue [] ns
          Usings = uses }
