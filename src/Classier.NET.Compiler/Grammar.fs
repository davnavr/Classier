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
    let parseNode (node: LineInfo -> 'Result -> Node<NodeValue>) (parser: Parser<'Result, unit>) stream =
        let reply = parser stream
        match reply.Status with
        | Ok ->
            let newNode = node (LineInfo (stream.Position.Line, stream.Position.Index)) reply.Result
            Reply (Ok, newNode, reply.Error)
        | _ ->
            Reply (reply.Status, reply.Error)

    let pwhitespace =
        anyOf [ ' '; '\t' ]
        |> many1Chars
        |> parseNode (fun pos str ->
            Node.terminal str Whitespace (pos.Advance str.Length))

    let pnewline =
        unicodeNewline
        |> parseNode (fun pos c ->
            Node.terminal (string c) Newline (pos.Advance 1))

    let pslcomment =
        pstring "//" .>>. restOfLine false
        |> parseNode (fun pos (str1, str2) ->
            let content = str1 + str2
            Node.terminal content Comment (pos.Advance content.Length))

    let parseIgnored allowSl =
        choice
            [
                pwhitespace;
                pnewline;
                if allowSl then
                    pslcomment
            ]
        |> opt

    let pidentifier =
        let palphabet =
            List.append [ 'a'..'z' ] [ 'A'..'Z' ]
            |> anyOf
        palphabet .>>. many (palphabet <|> digit)
        |> parseNode (fun pos (c, chars) ->
            let content = System.String(c :: chars |> List.toArray)
            Node.terminal content (Identifier (content.Split('.'))) (pos.Advance content.Length))

    //let pUseStatement =
    //    pstring "use" .>>. parseIgnored false .>>. pidentifier
    //    |> parseNode (fun oldState ((keyword, separator), id) ->
    //        let newState )

    pwhitespace
