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

open Classier.NET.Compiler.Node

type NodeValue =
    | CompilationUnit of {| Imports: seq<Node<NodeValue> * string>; Declaration: Node<NodeValue> |}
    | AccessModifier
    | Comment
    | Identifier
    | IdentifierChain
    | Keyword
    | Newline
    | Period
    | UseStatement
    | Whitespace

let parser: Parser<Node<NodeValue>, unit> =
    let parseNode (node: 'Result -> LineInfo -> Node<NodeValue>) (parser: Parser<'Result, unit>) =
        pipe2 parser (fun stream -> LineInfo (stream.Position.Line, stream.Position.Index) |> Reply) node

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
        |> parseNode (Node.terminal AccessModifier)

    let pWhitespace =
        anyOf [ ' '; '\t' ]
        |> many1Chars
        |> parseNode (Node.terminal Whitespace)

    let pNewline =
        unicodeNewline
        |> parseNode (fun c pos ->
            { Children = Seq.empty
              Content = string c
              Position = pos.NextLine
              Value = Newline })

    let pSlComment =
        pstring "//" .>>. restOfLine false
        |> parseNode (fun (str1, str2) ->
            Node.terminal Comment (str1 + str2))

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
        |>> fun (c, rest) -> c :: rest |> Array.ofList |> System.String
        |> parseNode (Node.terminal Identifier)

    let pIdentifierChain =
        pIdentifier
        .>>. many (
            (pIgnored false |> opt)
            .>>. (pstring "." |> parseNode (Node.terminal Period))
            .>>. (pIgnored false |> opt)
            .>>. pIdentifier)
        |>> fun (first, rest) ->
            first :: (rest
                |> List.map (fun (((sep1, period), sep2), next: Node<NodeValue>) ->
                    [
                        if sep1.IsSome then
                            sep1.Value
                        period
                        if sep2.IsSome then
                            sep2.Value
                        next
                    ])
                |> List.collect id)
        |> parseNode (Node.withChildren IdentifierChain)

    let pUseStatement =
        pstring "use"
        |> parseNode (Node.terminal Keyword)
        .>>. pIgnored false
        .>>. pIdentifierChain
        |>> fun ((useWord, sep), identifier) -> [ useWord; sep; identifier ]
        |> parseNode (Node.withChildren UseStatement)

    let pClassDef nested =
        pAccessModifier nested
        |> opt

    pUseStatement
