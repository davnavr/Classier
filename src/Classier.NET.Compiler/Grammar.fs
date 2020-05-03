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
    | CompilationUnit of {| Imports: seq<Identifier>; Declaration: Node<NodeValue> option |}
    | AccessModifier
    | Identifier of Identifier
    | Newline
    | Comment
    | Whitespace
and Identifier = seq<string>

let parser: Parser<Node<NodeValue>, unit> =
    let parseNode (node: 'Result -> LineInfo -> Node<NodeValue>) (parser: Parser<'Result, unit>) stream =
        let reply = parser stream
        match reply.Status with
        | Ok ->
            let newNode = node reply.Result (LineInfo (stream.Position.Line, stream.Position.Index))
            Reply (Ok, newNode, reply.Error)
        | _ ->
            Reply (reply.Status, reply.Error)

    let pwhitespace =
        anyOf [ ' '; '\t' ]
        |> many1Chars
        |> parseNode (Node.terminal Whitespace)

    let pnewline =
        unicodeNewline
        |> parseNode (fun c pos ->
            { Children = Seq.empty
              Content = string c
              Position = pos.NextLine
              Value = Newline })

    let pslcomment =
        pstring "//" .>>. restOfLine false
        |> parseNode (fun (str1, str2) ->
            Node.terminal Comment (str1 + str2))

    let parseIgnored allowSl =
        choice
            [
                pwhitespace;
                pnewline;
                if allowSl then
                    pslcomment
            ]

    let pidentifier =
        let palphabet =
            List.append [ 'a'..'z' ] [ 'A'..'Z' ]
            |> anyOf
        let psegment = palphabet .>>. many (palphabet <|> digit)
        psegment
        |> parseNode (fun _ _ ->
            invalidOp "Not implemented")

    //let pUseStatement =
    //    pstring "use" .>>. parseIgnored false .>>. pidentifier
    //    |> parseNode (fun oldState ((keyword, separator), id) ->
    //        let newState )

    pwhitespace
