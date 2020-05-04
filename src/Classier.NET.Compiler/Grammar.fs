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

open FParsec

open Classier.NET.Compiler.SyntaxNode

type NodeValue =
    | CompilationUnit of {| Imports: seq<SyntaxNode<NodeValue> * string>; Declaration: SyntaxNode<NodeValue> |}
    | AccessModifier
    | Comment
    | Identifier
    | IdentifierChain
    | Keyword
    | Newline
    | Period
    | UseStatement
    | Whitespace

let parser: Parser<SyntaxNode<NodeValue>, unit> =
    let parseNode (node: 'Result -> LinePos -> SyntaxNode<NodeValue>) (parser: Parser<'Result, unit>) =
        pipe2
            parser
            (fun stream ->
                LinePos (stream.Position.Line, stream.Position.Index)
                |> Reply)
            node

    let pAccessModifier full =
        [
            "public";
            "internal";
            if full then
                "protected";
                "private";
        ]
        |> Seq.map pstring
        |> choice
        |> parseNode (SyntaxNode.createToken AccessModifier)

    let pWhitespace =
        anyOf [ ' '; '\t' ]
        |> many1Chars
        |> parseNode (SyntaxNode.createToken Whitespace)

    let pNewline =
        unicodeNewline
        |> parseNode (fun c pos ->
            { Content = Token (string c)
              Position = pos.NextLine
              Value = Newline })

    let pSlComment =
        pstring "//" .>>. restOfLine false
        |> parseNode (fun (str1, str2) ->
            SyntaxNode.createToken Comment (str1 + str2))

    let pIgnored allowSl =
        choice
            [
                pWhitespace;
                pNewline;
                if allowSl then
                    pSlComment
            ]

    let pIdentifier =
        let palphabet =
            List.append [ 'a'..'z' ] [ 'A'..'Z' ]
            |> anyOf
        palphabet
        .>>. many (palphabet <|> digit)
        |>> fun (c, rest) ->
            c :: rest
            |> Array.ofList
            |> System.String
        |> parseNode (SyntaxNode.createToken Identifier)

    let pIdentifierChain = // TODO: Need to reverse the order, it things another identifier is next since it parses a newline at the end of "use my.identifier"
        many
            (pIdentifier
            .>>. (pIgnored false |> opt)
            .>>. (pstring "." |> parseNode (SyntaxNode.createToken Period))
            .>>. (pIgnored false |> opt))
        .>>. pIdentifier
        |>> fun (leading, last) ->
            Seq.append
                (leading
                |> Seq.collect (fun (((id, sep1), per), sep2) ->
                    [
                        id
                        if sep1.IsSome then
                            sep1.Value
                        per
                        if sep2.IsSome then
                            sep2.Value
                    ]))
                [ last ]
        |> parseNode (SyntaxNode.createNode IdentifierChain)

    let pUseStatement =
        pstring "use"
        |> parseNode (SyntaxNode.createToken Keyword)
        .>>. pIgnored false
        .>>. pIdentifierChain
        |>> fun ((useWord, sep), identifier) -> [ useWord; sep; identifier ]
        |> parseNode (SyntaxNode.createNode UseStatement)

    let pClassDef nested =
        pAccessModifier nested
        |> opt

    pUseStatement
