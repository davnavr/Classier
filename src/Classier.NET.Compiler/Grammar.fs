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
open Classier.NET.Compiler.SyntaxNode

// TODO: Switch to using Seq.append instead of List as it is much faster for larger collections.

type Visibility =
    | Public
    | Internal
    | Protected
    | Private

type MemberDef<'Flags> =
    { Name: string
      Visibility: Visibility
      Flags: 'Flags }

[<Flags>]
type ClassFlags =
    | None = 0
    | Abstract = 1
    | Inheritable = 2
    | Mutable = 4

[<Flags>]
type FuncFlags =
    | None = 0
    | Inline = 1

[<Flags>]
type MethodFlags =
    | None = 0
    | Inline = 1
    | Abstract = 2
    | Mutator = 4

type NodeValue =
    | CompilationUnit of
        {| Definitions: seq<SyntaxNode<NodeValue>>
           Imports: seq<seq<string>>
           Namespace: seq<string> option |}
    | AccessModifier of Visibility
    | BinLit
    | Block
    | ClassDef of MemberDef<ClassFlags>
    | Colon
    | Comma
    | Comment
    | DecLit
    | Expression
    | ExprAdd
    | ExprDiv
    | ExprSub
    | ExprMul
    | FieldDef of MemberDef<unit> * seq<string>
    | FuncDef of MemberDef<FuncFlags>
    | HexLit
    | Identifier of string
    | IdentifierFull of seq<string>
    | InterfaceDef of MemberDef<unit>
    | IntLit
    | Keyword
    | LCurlyBracket
    | LocalVarDef of string * seq<string>
    | LParen
    | MethodDef of MemberDef<MethodFlags> * seq<seq<Param>>
    | MethodHeader
    | ModuleDef of MemberDef<unit>
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

let parser: Parser<SyntaxNode<NodeValue>, unit> =
    let comma = charToken ',' Comma
    let keyword word = strToken word Keyword
    let lcurlybracket = charToken '{' LCurlyBracket
    let lparen = charToken '(' LParen
    let period = charToken '.' Period
    let rcurlybracket = charToken '}' RCurlyBracket
    let rparen = charToken ')' RParen
    let semicolon = charToken ';' Semicolon

    let accessModifier full =
        let modifiers =
            [
                "public", Visibility.Public
                "internal", Visibility.Internal
                if full then
                    "protected", Visibility.Protected
                    "private", Visibility.Private
            ]
        modifiers
        |> Seq.map (fun (str, vis) -> pstring str .>>. preturn vis)
        |> choice
        |> node (fun (str, vis) -> createToken (AccessModifier vis) str)
        <?> "access modifier"

    let block parser =
        lcurlybracket
        .>>. attempt parser
        .>>. rcurlybracket
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
            ]
        |> many

    let ignored1 = notEmpty ignored

    let parserSeq (parsers: seq<Parser<_, _>>) =
        fun stream ->
            let (nodes, (status, errors)) =
                parsers
                |> Seq.mapFold
                    (fun prev p ->
                        let (status, _) = prev
                        let result = p stream
                        match status with
                        | Ok ->
                            match result.Status with
                            | Ok -> Some result.Result, prev
                            | _ -> None, (result.Status, result.Error)
                        | _ -> None, prev)
                    (Ok, null)
            match status with
            | Ok -> Reply (Seq.choose id nodes)
            | _ -> Reply (status, errors)

    let identifier =
        asciiLetter
        .>>. many1Chars (asciiLetter <|> digit)
        |>> String.Concat
        |> node (fun name -> createToken (Identifier name) name)
        <?> "identifier"

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

    let identifierStatement word value =
        keyword word
        .>>. ignored1
        .>>. identifierFull
        |>> (fun ((wrd, sep), id) -> seq { wrd; yield! sep; id })
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

    let modifierOpt (parser: Parser<'a * 'a list, 'b>) =
        parser
        |> opt
        |>> (fun w ->
            match w with
            | Some (wordm, sep) -> seq { wordm; yield! sep }
            | None -> Seq.empty)

    let modifier word =
        keyword word
        .>>. ignored1
        |> modifierOpt

    let modifierChoice words =
        (*words
        |> Seq.map (fun (word, flag) ->
            keyword word .>> updateUserState (fun f -> f &&& flag))
        |> choice
        .>>. ignored1
        |> modifierOpt*)
        fail "why"

    let memberBlock =
        choice
            [
                ignored
            ]
        |> attempt
        |> many
        |>> Seq.collect id
        |> block

    let implements =
        ignored1
        .>>. keyword "implements"
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
        |> optnodes

    let classDef nested: Parser<SyntaxNode<NodeValue>, unit> = // TODO: Fix, need to use state to keep track of flags, but need some way of converting back to unit
        (*accessModifier nested
        .>> setUserState (ClassFlags.None)
        .>>. ignored1
        .>>. modifierChoice
                [
                    "abstract", ClassFlags.Abstract;
                    "inheritable", ClassFlags.Inheritable
                ]
        .>>. modifier "mutable"
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
        .>>. optnodes
                (ignored1
                .>>. keyword "extends"
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
        .>>. memberBlock
        |>> (fun ((name, def), body) -> name, seq { yield! def; body })
        .>>. getUserState
        |> node (fun ((name, nodes), flags) ->
            let classDef =
                ClassDef
                    ({ Name = name.ToString(); 
                       Flags = flags;
                       Visibility = Visibility.Public })
            createNode classDef nodes)*)
        fail "TEST"

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
                       |> Seq.tryHead |}
        createNode cu nodes)
    
