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

type Visibility =
    | Public = 0
    | Internal = 1
    | Protected = 2
    | Private = 3

[<Flags>]
type IntType =
    | Unsigned = 0
    | Signed = 1
    | Integer = 2
    | Long = 4

type FloatType =
    | Decimal
    | Double
    | Float

    static member Default = Double

type NumLiteral<'Type> =
    { Base: byte
      FracPart: char list
      IsNegative: bool
      IntPart: char list
      Type: 'Type }

type Definition =
    { Flags: Flags
      Name: string
      Visibility: Visibility }

type TypeName =
    | Identifier of Identifier list
    | Tuple of TypeName list
and Identifier =
    { Name: string
      GenericArgs: seq<GenericArg> }
and GenericArg = TypeName

type Expression =
    | FuncCall
    | MemberAccess of Identifier list
    | Nested of Expression
    | IntLiteral of NumLiteral<IntType>
    | FloatLiteral of NumLiteral<FloatType>

type Variable =
    { Definition: Definition
      Type: TypeName option
      Value: Expression option }

type TypeHeader =
    | Class of
        {| SuperClass: TypeName option |}
    | Interface
    | Module
    | Record
    | Union
and TypeDef =
    { Definition: Definition
      Header: TypeHeader
      Interfaces: TypeName list
      Members: MemberDef list }
and MemberDef =
    | Field of Variable
    | Function
    | NestedType of TypeDef
    | DUnionCase

type CompilationUnit =
    { Definitions: TypeDef list
      Namespace: string list
      Usings: string list list }

type ParserState =
    { Flags: Flags
      SymbolTable: unit
      Visibility: Visibility }

    static member Default =
        { Flags = Flags.None
          SymbolTable = ()
          Visibility = Visibility.Public }

// NOTE: Flags for classes might get overriden by its members.
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
    let lcurlybracket = skipChar '{'
    let lparen = skipChar '(' <?> "opening parenthesis"
    let ltsign = skipChar '<'
    let opequal = skipChar '='
    let period = skipChar '.'
    let rcurlybracket = skipChar '}'
    let rparen = skipChar ')' <?> "closing parenthesis"
    let semicolon = skipChar ';' <?> "semicolon"

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

    let identifier, identifierRef = createParserForwardedToRef<Identifier, _>()
    let typeName, typeNameRef = createParserForwardedToRef<TypeName, _>()

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
        |> opt
        <?> "type annotation"

    let modifier word flags =
        skipString word
        |> optional
        .>> ignored
        .>> setFlags flags

    let separator p = ignored >>. p .>> ignored

    let block ps =
        lcurlybracket
        |> attempt
        >>. ignored
        >>. (choice ps
            .>> ignored
            |> many)
        .>> rcurlybracket

    let genericArgs =
        sepBy1 typeName (separator comma)
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

    let expression =
        let decimalChars = [ '0'..'9' ]
        let expr = OperatorPrecedenceParser<_,_,_> ()

        expr.TermParser <-
            choice
                [
                    identifierList |>> MemberAccess;
                    
                    opt (pchar '-')
                    .>>.
                        (pchar '0'
                        |> attempt
                        >>. choice
                            [
                                anyOf [ 'b'; 'B' ] >>. preturn [ '0'; '1'; ];
                                anyOf [ 'x'; 'X' ] >>. preturn (decimalChars @ [ 'a'..'z' ] @ [ 'A'..'Z' ]);
                            ]
                        <|> preturn decimalChars
                        <|> (pchar '.' >>. preturn []))
                    >>= (fun (neg, chars) ->
                        let nbase = byte(chars.Length)
                        let digits c =
                            anyOf c
                            |> attempt
                            .>> (skipMany (pchar '_') <?> "digit separator")
                            |> many1
                        let decimalDigits = digits decimalChars

                        match chars.Length with
                        | 0 ->
                            decimalDigits
                            |>> fun digits ->
                                FloatLiteral
                                    { Base = byte(10)
                                      FracPart = digits
                                      IsNegative = neg.IsSome
                                      IntPart = []
                                      Type = FloatType.Default }
                        | 10 ->
                            decimalDigits
                            .>>. opt
                                (pchar '.'
                                |> attempt
                                >>. decimalDigits)
                            |>> fun (idigits, fdigits) ->
                                match fdigits with
                                | Some _ ->
                                    FloatLiteral
                                        { Base = nbase
                                          FracPart = fdigits.Value
                                          IsNegative = neg.IsSome
                                          IntPart = idigits
                                          Type = FloatType.Default }
                                | None ->
                                    IntLiteral
                                        { Base = nbase
                                          FracPart = []
                                          IsNegative = neg.IsSome
                                          IntPart = idigits
                                          Type = IntType.Integer }
                        | _ ->
                            digits chars
                            |>> fun digits ->
                                IntLiteral
                                    { Base = nbase
                                      FracPart = []
                                      IsNegative = false
                                      IntPart = digits
                                      Type = IntType.Integer })
                    <?> "numeric literal";
                ]

        expr.ExpressionParser <?> "expression"

    let variableDef =
        setUserState { ParserState.Default with Visibility = Visibility.Private }
        .>> skipString "let"
        .>> ignored1
        >>. identifierStr
        .>>. typeAnnotation
        .>> ignored
        .>>. opt
            (opequal
            |> attempt
            >>. ignored
            >>. expression)
        .>> semicolon
        .>>. getUserState
        |>> fun (((name, tann), expr), state) ->
            { Definition =
                { Name = name
                  Flags = state.Flags
                  Visibility = state.Visibility }
              Value = expr
              Type = tann }

    typeNameRef :=
        choice
            [
                identifierFull

                between
                    lparen
                    rparen
                    (sepBy typeName (separator comma))
                |>> Tuple
                <?> "tuple"
            ]
        <?> "type name"

    identifierRef :=
        identifierStr
        .>> ignored
        .>>. genericArgs
        |>> fun (name, gparams) ->
            { Name = name
              GenericArgs = gparams }
    
    let nsStatementL word label =
        pstring word
        |> attempt
        >>. ignored1
        >>. sepBy identifierStr (separator period)
        .>> ignored
        .>> semicolon
        <?> label

    let genericParams =
        let genericParam =
            identifierStr
        ltsign
        |> attempt
        >>. sepBy1 genericParam (separator comma)
        .>> gtsign
        |> opt
        |>> Option.defaultValue []

    let typeDef word =
        skipString word
        |> attempt
        >>. ignored1
        >>. getUserState

    let fieldDef =
        variableDef
        |>> Field
        <?> "field definition"

    let implements =
        ignored1
        .>> skipString "implements"
        |> attempt
        .>> ignored1
        >>. sepBy1 identifierFull (separator comma)
        |> opt
        |>> Option.defaultValue []
        <?> "interface implementations"

    let classDef =
        typeDef "class"
        .>>. identifierStr
        .>>. genericParams
        .>>. (ignored1
             .>> skipString "extends"
             |> attempt
             .>> ignored1
             >>. identifierFull
             |> opt)
        .>>. implements
        .>> ignored
        .>>. block
            [
                fieldDef
            ]
        <?> "class definition"
        |>> fun (((((state, name), gparams), superclass), iimpls), members) ->
            { Definition =
                { Name = name
                  Flags = state.Flags
                  Visibility = state.Visibility }
              Header = Class {| SuperClass = superclass |}
              Interfaces = iimpls
              Members = members }

    let moduleDef =
        typeDef "module"
        .>>. identifierStr
        .>>. genericParams
        .>> ignored
        .>>. block
            [
                fieldDef
            ]
        <?> "module definition"
        |>> fun (((state, name), gparams), members) ->
            { Definition =
                { Name = name
                  Flags = state.Flags 
                  Visibility = state.Visibility }
              Header = Module
              Interfaces = []
              Members = members }

    let typeDefs =
        accessModifier Visibility.Internal
        >>. ignored1
        >>. modifier "inheritable" Flags.Inheritable // TODO: use >>= to check that these modifiers are only applied on classes.
        >>. modifier "abstract" Flags.Abstract
        >>. choice
            [
                classDef
                moduleDef
            ]

    ignored
    >>. opt (nsStatementL "namespace" "namespace declaration")
    .>> ignored
    .>>. many (nsStatementL "use" "use statement" .>> ignored)
    .>>. sepBy1 typeDefs ignored
    |>> fun ((ns, uses), defs) ->
        { Definitions = defs
          Namespace = Option.defaultValue [] ns
          Usings = uses }
