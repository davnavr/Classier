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

module Classier.NET.Compiler.Grammar

open System

open FParsec

open Classier.NET.Compiler.NodeParsers
open Classier.NET.Compiler.SeqParsers
open Classier.NET.Compiler.SyntaxNode

[<Flags>]
type Flags =
    | None = 0
    | Public = 0
    | Internal = 1
    | Protected = 2
    | Private = 3
    | VisibilityMask = 3
    | Inline = 4
    | Abstract = 8
    /// Indicates that a class, field, or local variable is mutable.
    | Mutable = 16
    /// Indicates that a method or function changes the value of a field.
    | Mutator = 32
    /// Indicates that a class can have a subclass.
    | Inheritable = 64

type Definition =
    { Name: string
      Flags: Flags }
    
    member this.Visibility =
        this.Flags ||| Flags.VisibilityMask

type NodeValue =
    | CompilationUnit of
        {| Definitions: seq<SyntaxNode<NodeValue>>
           Imports: seq<seq<string>>
           Namespace: seq<string> |}
    | AccessModifier of Flags
    | BinLit
    | Block
    | ClassDef of Definition
    | Colon
    | Comma
    | Comment
    | DecLit
    | Expression
    | ExprAdd
    | ExprDiv
    | ExprSub
    | ExprMul
    | FieldDef of FieldOrVar
    | FuncDef of Definition
    | HexLit
    | Identifier of string
    | IdentifierFull of seq<string>
    | InterfaceDef of Definition
    | IntLit
    | Keyword
    | LCurlyBracket
    | LocalVarDef of FieldOrVar
    | LParen
    | MethodDef of Definition * seq<seq<Param>>
    | MethodHeader
    | ModuleDef of Definition
    | NamespaceDef of seq<string>
    | Newline
    | OpAdd
    | OpDiv
    | OpEqual
    | OpSub
    | OpMul
    | Param of Param
    | ParamTuple of seq<Param>
    | ParamSet of seq<seq<Param>>
    | Period
    | RCurlyBracket
    | RParen
    | Semicolon
    | TypeAnnotation of seq<string>
    | UseStatement of seq<string>
    | Whitespace
and Param = { Name: string; ParamType: seq<string> }
and FieldOrVar =
    { Definition: Definition
      ValueType: seq<string>
      Value: SyntaxNode<NodeValue> option }

type TypeKind =
    | Class = 0
    | Module = 1

let parser: Parser<SyntaxNode<NodeValue>, Flags> =
    let colon = charToken ':' Colon
    let comma = charToken ',' Comma
    let keyword word = strToken word Keyword
    let lcurlybracket = charToken '{' LCurlyBracket
    let lparen = charToken '(' LParen
    let opequal = charToken '=' OpEqual
    let period = charToken '.' Period
    let rcurlybracket = charToken '}' RCurlyBracket
    let rparen = charToken ')' RParen
    let semicolon = charToken ';' Semicolon

    let accessModifier full =
        let modifiers =
            [
                "public", Flags.Public
                "internal", Flags.Internal
                if full then
                    "protected", Flags.Protected
                    "private", Flags.Private
            ]
        modifiers
        |> Seq.map (fun (str, vis) ->
            pstring str
            |> attempt
            .>>. preturn vis
            .>> setUserState vis)
        |> choice
        |> node (fun (str, vis) -> createToken (AccessModifier vis) str)
        <?> "access modifier"

    let block parser =
        lcurlybracket
        .>>. attempt parser
        .>>. (rcurlybracket <?> "closing bracket")
        |>> (fun ((lc, content), rc) -> seq { lc; yield! content; rc })
        |> node (createNode Block)

    let ignored =
        choice
            [
                anyOf [ ' '; '\t' ]
                <?> "whitespace"
                |> many1Chars
                |> token Whitespace

                newline
                |> node (fun c pos ->
                    { Content = Token (string c)
                      Position = pos.NextLine
                      Value = Newline })

                pstring "//" .>>. restOfLine false
                |>> String.Concat
                |> token Comment
                <?> "single-line comment"
            ]
        |> attempt
        |> many

    let ignored1 = notEmpty ignored

    let modifierChoice words =
        words
        |> Seq.map (fun (word, flag) ->
            keyword word .>> updateUserState (fun f -> f ||| flag))
        |> choice
        .>>. ignored1
        |> opt
        |>> (fun w ->
            match w with
            | Some (wordm, sep) -> seq { wordm; yield! sep }
            | None -> Seq.empty)

    let modifier word flag =
        modifierChoice [ word, flag ]

    let modifierMutable = modifier "mutable" Flags.Mutable

    let identifier =
        asciiLetter
        .>>. many1Chars (asciiLetter <|> digit)
        |>> String.Concat
        |> node (fun name -> createToken (Identifier name) name)
        <??> "identifier"

    let identifierFull =
        many
            (identifier .>>. ignored .>>. period .>>. ignored
            |> attempt
            |>> fun (((name, sep1), per), sep2) ->
                seq {
                    name
                    yield! sep1
                    per
                    yield! sep2
                })
        |>> Seq.collect id
        .>>. identifier
        |>> fun (leading, last) -> seq { yield! leading; last }
        |> node (fun nodes ->
            let names =
                nodes
                |> Seq.choose (fun node ->
                    match node.Value with
                    | Identifier name -> Some name
                    | _ -> None)
            createNode (IdentifierFull names) nodes)
    
    let typeAnnotation =
        ignored
        .>>. colon
        .>>. ignored
        .>>. identifierFull
        |>> (fun (((sep1, col), sep2), typeName) ->
            typeName,
            seq {
                yield! sep1
                col
                yield! sep2
                typeName
            })
        |> node (fun (typeName, nodes) ->
            let valueType =
                match typeName.Value with
                | IdentifierFull names -> names
                | _ -> Seq.empty
            createNode (TypeAnnotation valueType) nodes)

    let typeNamePair =
        identifier
        .>>. typeAnnotation

    let expression: Parser<SyntaxNode<NodeValue>, Flags> =
        let numSuffixes suffixes =
            suffixes
            |> List.map pstringCI
            |> choice
            |> opt
            |>> Option.defaultValue String.Empty
            <?> "numeric suffix"
        let decSuffixes = numSuffixes [ "d"; "f"; "m"; ]
        let intSuffixes = numSuffixes [ "l"; "u"; "ul"; "lu" ]
        let intDigits =
            digit
            .>>. manyChars (digit <|> pchar '_')
            |>> fun (leading, trailing) ->
                sprintf "%c%s" leading trailing

        let expr = OperatorPrecedenceParser<_,_,_> ()

        [
            ExprAdd, OpAdd, "+", 1, Associativity.Left;
            ExprSub, OpSub, "-", 1, Associativity.Left;
            ExprMul, OpMul, "*", 5, Associativity.Left;
            ExprDiv, OpDiv, "/", 5, Associativity.Left;
        ]
        |> List.map (fun (exprType, operandType, symbol, prec, assoc) ->
            let mapping op (expr1: SyntaxNode<NodeValue>) (expr2: SyntaxNode<NodeValue>) =
                let opNode = createToken operandType symbol expr1.Position
                let nodes =
                    seq {
                        expr1
                        opNode
                        yield! op
                        expr2
                    }
                createNode exprType nodes expr1.Position
            InfixOperator (symbol, ignored, prec, assoc, (), mapping))
        |> List.iter expr.AddOperator

        expr.TermParser <-
            choice
                [
                    identifierFull

                    pstring "-"
                    |> opt
                    |>> Option.defaultValue String.Empty
                    .>>. choice
                        [
                            pstringCI "0x"
                            .>>. hex
                            .>>. manyChars (hex <|> pchar '_')
                            .>>. intSuffixes
                            |>> (fun (((prefix, first), rest), suffix) ->
                                sprintf "%s%c%s%s" prefix first rest suffix)
                            .>>. preturn HexLit
                            <?> "hexadecimal literal";

                            pstringCI "0b"
                            .>>. anyOf [ '0'; '1' ]
                            .>>. manyChars (anyOf [ '0'; '1'; '_' ])
                            .>>. intSuffixes
                            |>> (fun (((prefix, first), rest), suffix) ->
                                sprintf "%s%c%s%s" prefix first rest suffix)
                            .>>. preturn BinLit
                            <?> "binary literal";

                            intDigits
                            .>>. pstring "."
                            .>>. intDigits
                            .>>. decSuffixes
                            |>> (fun (((intp, per), decp), suffix) -> intp + per + decp + suffix)
                            .>>. preturn DecLit
                            |> attempt
                            <?> "numeric literal"

                            intDigits
                            .>>. intSuffixes
                            |>> (fun (digits, suffix) -> digits + suffix)
                            .>>. preturn IntLit
                            <?> "integer literal";
                        ]
                    |>> (fun (sign, (node, numType)) -> sign + node, numType)
                    |> node (fun (content, numType) -> createToken numType content);

                    lparen
                    .>>. ignored
                    .>>. expr.ExpressionParser
                    .>>. ignored
                    .>>. rparen
                    |>> (fun ((((lp, sep1), exp), sep2), rp) ->
                        seq {
                            lp
                            yield! sep1
                            exp
                            yield! sep2
                            rp
                        })
                    |> node (createNode Expression)
                ]
            .>>. ignored
            |> node (fun (expr, sep) pos ->
                if Seq.isEmpty sep
                then expr
                else createNode Expression (seq { expr; yield! sep }) pos)

        expr.ExpressionParser <?> "expression"

    let identifierStatement word value =
        keyword word
        .>>. ignored1
        .>>. identifierFull
        .>>. ignored
        .>>. semicolon
        |>> (fun ((((wrd, sep1), id), sep2), sep3) ->
            seq {
                wrd
                yield! sep1
                id
                yield! sep2
                sep3
            })
        |> node (fun nodes ->
            let id =
                nodes
                |> Seq.choose (fun node ->
                    match node.Value with
                    | IdentifierFull names -> Some names
                    | _ -> None)
                |> Seq.collect id
            createNode (value id) nodes)

    let useStatements =
        ignored
        .>>. (identifierStatement "use" UseStatement <?> "use statement")
        .>>. ignored1
        |> attempt
        |>> (fun ((sep1, st), sep2) -> seq { yield! sep1; st; yield! sep2 })
        |> many
        |>> Seq.collect id
        |> optseq

    let variable value =
        keyword "let"
        .>>. ignored1
        .>>. modifierMutable
        .>>. typeNamePair
        .>>. optseq
                (ignored
                .>>. opequal
                .>>. ignored
                .>>. expression
                |>> fun (((sep1, eq), sep2), expr) ->
                    seq {
                        yield! sep1
                        eq
                        yield! sep2
                        expr
                    })
        .>>. semicolon
        .>>. getUserState
        |>> (fun ((((((wordl, sep), wordm), (name, valueType)), value), sc), flags) ->
            flags,
            valueType,
            name,
            seq {
                wordl
                yield! sep
                yield! wordm
                name
                valueType
                yield! value
                sc
            })
        |> node (fun (flags, valueType, name, nodes) ->
            let varType =
                match valueType.Value with
                | TypeAnnotation names -> names
                | _ -> Seq.empty
            let varName =
                match name.Value with
                | Identifier name -> name
                | _ -> String.Empty
            let varValue =
                nodes
                |> Seq.tryPick (fun node ->
                    match node.Value with
                    | Expression -> Some node
                    | _ -> None)
                
            createNode (value flags varType varName varValue) nodes)

    let memberBlock kind =
        ignored
        .>>. choice
            [
                if kind >= TypeKind.Class then
                    variable
                        (fun flags valueType name value ->
                            FieldDef
                                ({ Definition =
                                    {
                                        Name = name
                                        Flags = flags
                                    }
                                   ValueType = valueType
                                   Value = value }))
                    <?> "field definition"
            ]
        .>>. ignored
        |> attempt
        |>> (fun ((sep1, node), sep2) ->
            seq {
                yield! sep1
                node
                yield! sep2
            })
        |> many
        |>> Seq.collect id
        |> block

    let implements =
        ignored1
        .>>. keyword "implements"
        |> attempt
        .>>. ignored1
        .>>. many1
                (identifierFull
                .>>. ignored
                .>>. comma
                .>>. ignored
                |> attempt
                |>> fun (((name, sep1), c), sep2) ->
                    seq {
                        name
                        yield! sep1
                        c
                        yield! sep2
                    })
        |>> (fun (((sep1, wordi), sep2), interfaces) ->
            seq {
                yield! sep1
                wordi
                yield! sep2
                yield! Seq.collect id interfaces
            })
        |> optseq

    let classDef nested =
        accessModifier nested
        .>>. ignored1
        .>>. modifierChoice
                [
                    "abstract", Flags.Abstract;
                    "inheritable", Flags.Inheritable;
                ]
        .>>. modifierMutable
        |>> (fun (((acc, sep), worda), wordm) ->
            seq {
                acc
                yield! sep
                yield! worda
                yield! wordm
            })
        .>>. ignored
        .>>. keyword "class"
        |> attempt
        .>>. ignored1
        .>>. identifier
        |>> (fun ((((modifiers, sep1), wordc), sep2), name) ->
                name,
                seq {
                    yield! modifiers
                    yield! sep1
                    wordc
                    yield! sep2
                    name
                })
        .>>. optseq
                (ignored1
                .>>. keyword "extends"
                |> attempt
                .>>. ignored1
                .>>. identifierFull
                |>> fun (((sep1, worde), sep2), id) ->
                    seq {
                        yield! sep1
                        worde
                        yield! sep2
                        id
                    })
        .>>. implements
        .>>. ignored
        |>> (fun ((((name, modifiers), extend), iimpl), sep) ->
                name,
                seq {
                    yield! modifiers
                    yield! extend
                    yield! iimpl
                    yield! sep
                })
        .>>. memberBlock TypeKind.Class
        |>> (fun ((name, def), body) -> name, seq { yield! def; body })
        .>>. getUserState
        |> node (fun ((name, nodes), flags) ->
            let classDef =
                ClassDef
                    ({ Name = name.ToString(); 
                       Flags = flags; })
            createNode classDef nodes)
        <?> "class definition"

    useStatements
    .>>. opt (identifierStatement "namespace" NamespaceDef <?> "namespace definition")
    .>>. useStatements
    .>>. many
            (choice
                [
                    classDef false
                ]
             .>>. ignored
             |>> fun (def, sep) -> seq { def; yield! sep })
    |>> (fun (((use1, ns), use2), defs) ->
        seq {
            yield! use1
            if ns.IsSome then
                ns.Value
            yield! use2
            yield! defs |> Seq.collect id
        })
    .>> eof
    |> node (fun nodes ->
        let cu =
            CompilationUnit
                {| Definitions =
                       nodes
                       |> Seq.where (fun node ->
                           match node.Value with
                           | ClassDef _ | ModuleDef _ -> true
                           | _ -> false)
                   Imports =
                       nodes
                       |> Seq.choose (fun node ->
                           match node.Value with
                           | UseStatement import -> Some import
                           | _ -> None)
                   Namespace =
                       nodes
                       |> Seq.choose (fun node ->
                           match node.Value with
                           | NamespaceDef ns -> Some ns
                           | _ -> None)
                       |> Seq.tryHead 
                       |> Option.defaultValue Seq.empty |}
        createNode cu nodes)
