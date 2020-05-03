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

type ParseState = // TODO: Get rid of this? We can track position info using the given CharStream.State object, and can use anyChar when parsing multiline comments.
    { InComment: bool
      Position: LineInfo }
    with
        static member Default = { InComment = false; Position = LineInfo.Default }

let parser: Parser<Node<NodeValue>, ParseState> =
    let updatePos oldState (str: string) =
        { InComment = oldState.InComment
          Position = oldState.Position + str.Length }

    let parseNode (node: ParseState -> 'Result -> ParseState * Node<NodeValue>) (parser: Parser<'Result, ParseState>) stream =
        let reply = parser stream
        match reply.Status with
        | Ok ->
            let (newState, newNode) = node stream.UserState reply.Result
            stream.UserState <- newState
            Reply (Ok, newNode, reply.Error)
        | _ ->
            Reply (reply.Status, reply.Error)

    let pwhitespace =
        anyOf [ ' '; '\t' ]
        |> many1Chars
        |> parseNode (fun oldState str ->
            let newState = updatePos oldState str
            newState, Node.terminal str Whitespace newState.Position)

    let pnewline =
        unicodeNewline
        |> parseNode (fun oldState c ->
            let newState = { InComment = oldState.InComment; Position = oldState.Position.NextLine }
            newState, Node.terminal (string c) Newline newState.Position)

    let pslcomment =
        pstring "//" .>>. restOfLine false
        |> parseNode (fun oldState (str1, str2) ->
            let content = str1 + str2
            let newState = updatePos oldState content
            newState, Node.terminal content Comment newState.Position)

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
        |> parseNode (fun oldState (c, chars) ->
            let content = System.String(c :: chars |> List.toArray)
            let newState = updatePos oldState content
            newState, Node.terminal content (Identifier (content.Split('.'))) newState.Position)

    //let pUseStatement =
    //    pstring "use" .>>. parseIgnored false .>>. pidentifier
    //    |> parseNode (fun oldState ((keyword, separator), id) ->
    //        let newState )

    pwhitespace
