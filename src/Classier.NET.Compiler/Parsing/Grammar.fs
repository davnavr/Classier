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
    | None = 0
    | Inline = 1
    | Abstract = 2
    /// Indicates that a class, field, or local variable is mutable,
    /// or that a method changes the value of a field.
    | Mutable = 4
    /// Indicates that a class can have a subclass.
    | Inheritable = 8

type Visibility = // TODO: Merge visibility and flags together?
    | Public = 0
    | Internal = 1
    | Protected = 2
    | Private = 3

[<Flags>]
type IntType = // TODO: Merge the number types together?
    | Signed = 0
    | Unsigned = 1
    | Integer = 2
    | Long = 4

type FloatType =
    | Decimal = 0
    | Double = 1
    | Float = 2

type NumLiteral<'Type> =
    { Base: byte
      FracPart: char list
      IntPart: char list
      Type: 'Type }

type Definition =
    { Flags: Flags
      Name: string
      Visibility: Visibility }

type TypeName =
    | Identifier of Identifier list
    | Inferred
    | Tuple of TypeName list
    | Union of TypeName list
and Identifier =
    { Name: string
      GenericArgs: seq<GenericArg> }
and GenericArg = TypeName

type Expression =
    | Assignment of
        {| Target: Expression
           Value: Expression |}
    | FloatLit of NumLiteral<FloatType>
    | FuncCall of
        {| Arguments: Expression list list
           Target: Expression |}
    | IdentifierRef of Identifier
    | IfExpr of IfStatement
    | IntLit of NumLiteral<IntType>
    | MemberAccess of Expression * Identifier
    | Nested of Expression
    | TupleLit of Expression list
    | UnitLit
and IfStatement =
    { Condition: Expression
      Choice1: Statement list
      Choice2: Statement list }
and Statement =
    | Empty
    | If of IfStatement
    /// An expression whose result is evaluated then discarded.
    | IgnoredExpr of Expression
    | LocalVar of Variable
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
      RequiredSuperClass: TypeName option
      RequiredInterfaces: TypeName list
      Variance: GenericVariance }

type Function =
    { Body: Statement list
      FuncDef: Definition
      GenericParams: GenericParam list
      Parameters: Param list list
      ReturnType: TypeName }

type TypeHeader =
    | Class of TypeName option
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
      Usings: string list list }

[<Flags>]
type ParseType =
    | Any = 0
    | Class = 1
    | DUnion = 2
    | Interface = 4
    | Module = 8
    | Record = 16
    | FieldOrVar = 32
    | Function = 64
    | Method = 128
    | DUnionCase = 256
    | Property = 512

type ParserState =
    { Flags: Flags
      Symbols: SymbolTable
      Visibility: Visibility }

    static member Default =
        { Flags = Flags.None
          Symbols = SymbolTable(Seq.empty)
          Visibility = Visibility.Public }

    member this.CreateDefinition name =
        { Name = name
          Flags = this.Flags
          Visibility = this.Visibility }

let parser: Parser<CompilationUnit, ParserState> =
    let setFlags flags =
        updateUserState
            (fun (st: ParserState) -> { st with Flags = st.Flags ||| flags })
    let setVisibility vis =
        updateUserState
            (fun (st: ParserState) -> { st with Visibility = vis })

    let colon = skipChar ':'
    let comma = skipChar ','
    let gtsign = skipChar '>'
    let lcurlybracket = skipChar '{' <?> "opening bracket"
    let lparen = skipChar '(' <?> "opening parenthesis"
    let ltsign = skipChar '<'
    let opequal = skipChar '='
    let period = skipChar '.'
    let rcurlybracket = skipChar '}' <?> "closing bracket"
    let rparen = skipChar ')' <?> "closing parenthesis"
    let semicolon = skipChar ';' <?> "semicolon"
    let lambdaOperator = skipString "=>" |> attempt <?> "lambda operator"

    let accessModifier lowest =
        let modifiers =
            [
                "public", Visibility.Public
                "internal", Visibility.Internal
                "protected", Visibility.Protected
                "private", Visibility.Private
            ]
            |> Seq.filter (fun (_, vis) -> vis <= lowest)
        modifiers
        |> Seq.map (fun (str, vis) ->
            skipString str .>> setVisibility vis)
        |> choice
        <?> "access modifier"

    let classDef, classDefRef = createParserForwardedToRef<TypeDef,_>()
    let identifier, identifierRef = createParserForwardedToRef<Identifier,_>()
    let ifStatement, ifStatementRef = createParserForwardedToRef<IfStatement, _>()
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

    let modifiers ptype = // TODO: Move modifiers elsewhere? Want to get rid of SymbolType as we have too many enums
        let modifier name flag target =
            if target = ParseType.Any || target.HasFlag(ptype) then
                skipString name
                >>. ignored1
                |> attempt
                >>. setFlags flag
                |> optional
            else
                preturn ()
        
        modifier "abstract" Flags.Abstract ParseType.Any
        >>. modifier "inheritable" Flags.Inheritable ParseType.Class
        >>. modifier "mutable" Flags.Mutable (ParseType.Class ||| ParseType.FieldOrVar)
        >>. modifier "inline" Flags.Inline (ParseType.Method ||| ParseType.Function)
        >>. modifier "mutator" Flags.Mutable ParseType.Method

    let separator p = ignored >>. p .>> ignored

    let block ps = // TODO: Don't use choice, let ps be any parser
        lcurlybracket
        |> attempt
        >>. ignored
        >>. (choice ps
            .>> ignored
            |> many)
        .>> rcurlybracket

    let genericArgs =
        ltsign
        |> attempt
        >>. sepBy1 typeName (separator comma)
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
            let target = MemberAccess (targetExpr, { Name = name; GenericArgs = Seq.empty })
            FuncCall {| Arguments = args; Target = target |}

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
                InfixOperator<_,_,_>("<-", ignored, 100, Associativity.Left, fun target value -> Assignment {| Target = target; Value = value |}); // TODO: Maybe use a different symbol for assignment?
            ]
        |> Seq.iter expr.AddOperator

        let tuple =
            lparen
            |> attempt
            >>. ignored
            >>. sepBy exprParser (separator comma)
            .>> rparen

        expr.TermParser <-
            choice
                [
                    ifStatement
                    |>> IfExpr
                    <?> "if expression";

                    identifier |>> IdentifierRef;

                    tuple
                    <?> "tuple"
                    |>> (fun ex ->
                        match ex with
                        | [] -> UnitLit
                        | [ nested ] -> nested
                        | _ -> TupleLit ex);

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
                                        "u", IntType.Integer ||| IntType.Unsigned
                                        "l", IntType.Long
                                        "ul", IntType.Long ||| IntType.Unsigned
                                        "lu", IntType.Long ||| IntType.Unsigned
                                    ]
                                    IntType.Integer
                                |>> fun intType ->
                                    IntLit
                                        { Base = byte(chars.Length)
                                          FracPart = []
                                          IntPart = idigits
                                          Type = intType }
                            else
                                suffixes
                                    [
                                        "d", FloatType.Double
                                        "f", FloatType.Float
                                        "m", FloatType.Decimal
                                    ]
                                    FloatType.Double
                                |>> fun floatType ->
                                    FloatLit
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

    let variableDef = // TODO: Use var or val instead, which might be more familiar to C# and Java developers
        skipString "let"
        .>> ignored1
        |> attempt
        .>> setUserState { ParserState.Default with Visibility = Visibility.Private }
        .>> modifiers ParseType.FieldOrVar
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
                  Flags = state.Flags
                  Visibility = state.Visibility }
              Value = expr
              Type = tann }

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
        |> attempt
        >>= fun () ->
            let pcondition =
                ignored
                >>. lparen
                >>. ignored
                >>. expression
                .>> rparen
                .>> ignored

            pcondition
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
                            |>> fun e -> [ If e ]

                            ignored
                            >>. statementBlock;
                        ]

                    preturn []
                ]
            |>> fun ((condition, body), rest) ->
                { Condition = condition
                  Choice1 = body
                  Choice2 = rest }

    statementBlockRef :=
        block
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

                ifStatement |>> If <?> "if statement";
                
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
    
    let nsStatementL word label =
        pstring word
        |> attempt
        >>. ignored1
        >>. sepBy identifierStr (separator period)
        .>> ignored
        .>> semicolon
        <?> label

    let extends =
        ignored1
        .>> skipString "extends"
        |> attempt
        .>> ignored1
        >>. identifierFull
        |> opt

    let implements =
        ignored1
        .>> skipString "implements"
        |> attempt
        .>> ignored1
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

    let fieldDef =
        variableDef
        |>> Field
        <?> "field definition"

    let functionDef ftype =
        modifiers ftype
        >>. getUserState
        .>>. identifierStr
        .>> ignored
        .>>. genericParams
        .>> ignored
        .>>. (paramTuple .>> ignored |> many1 <?> "parameter list")
        .>>. typeAnnotationOpt
        .>> ignored
        .>>. (choice
            [
                statementBlock;

                lambdaOperator
                |> attempt
                >>. ignored
                >>. expression
                |>> Return
                |>> List.singleton
                .>> ignored
                .>> semicolon;

                semicolon >>. preturn [];
            ]
            <?> "function body")
        |>> fun (((((st, name), gparams), fparams), retType), body) ->
            Function
                { Body = body
                  FuncDef =
                    { Name = name
                      Flags = st.Flags
                      Visibility = st.Visibility }
                  GenericParams = gparams
                  Parameters = fparams
                  ReturnType = retType }

    let methodDef = functionDef ParseType.Method <?> "method definition";

    let typeDef word ptype =
        modifiers ptype
        >>. skipString word
        >>. ignored1
        |> attempt
        >>. getUserState
        |>> fun state -> state.CreateDefinition

    let recordDef =
        typeDef "data" ParseType.Record
        .>>. identifierStr
        .>>. genericParams
        .>> ignored
        .>>. block
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
                              Flags = Flags.None
                              Visibility = Visibility.Public }
                          Value = None
                          Type = ftype }

                accessModifier Visibility.Private
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
        typeDef "union" ParseType.DUnion
        .>>. identifierStr
        .>>. genericParams
        .>> ignored
        .>>. block
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
        typeDef "interface" ParseType.Interface
        .>>. identifierStr
        .>>. genericParams
        .>>. implements
        .>> ignored
        .>>. block
            [
                accessModifier Visibility.Internal
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
        typeDef "class" ParseType.Class
        .>>. identifierStr
        .>>. genericParams
        .>>. extends
        .>>. implements
        .>> ignored
        .>>. block
            [
                fieldDef;

                accessModifier Visibility.Private
                >>. ignored1
                >>. choice
                    [
                        classDef |>> NestedType
                        methodDef
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
        typeDef "module" ParseType.Module
        .>>. identifierStr
        .>>. genericParams
        .>> ignored
        .>>. block
            [
                fieldDef;
                
                accessModifier Visibility.Private
                >>. ignored1
                >>. choice
                    [
                        recordDef |>> NestedType
                        unionDef |>> NestedType
                        interfaceDef |>> NestedType
                        classDef |>> NestedType
                        functionDef ParseType.Function <?> "function definition"
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
    >>. opt (nsStatementL "namespace" "namespace declaration")
    .>> ignored
    .>>. many (nsStatementL "use" "use statement" .>> ignored)
    .>>. (accessModifier Visibility.Internal
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
