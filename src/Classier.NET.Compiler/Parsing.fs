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

module Classier.NET.Compiler.Parsing

open Classier.NET.Compiler.Lexing

/// Provides line and position information for a token.
type ParsedToken<'T> =
    {
        Token: Token<'T>
        /// Zero-based index indicating the line that this token is on.
        Line: int
        Pos: int
    }

type Node<'T> =
    {
        Tokens: seq<ParsedToken<'T>>
        Children: seq<Node<'T>>
        Type: 'T
    }

/// Turns a sequence of tokens into a sequence of nodes.
type Parser<'T> = Parser of (seq<ParsedToken<'T>> -> seq<Node<'T>>)

/// Adds line number and position information to a sequence of tokens.
let lineInfo (tokens: seq<Token<'T>>) isNewline =
    let nextToken (newline, line, pos) token =
        let (nextLine, nextPos) =
            if newline
            then line + 1, 0
            else line, pos //+ 1
        { Token = token; Line = nextLine; Pos = nextPos }, (isNewline token, nextLine, nextPos + token.Content.Length)
    let (parsedTokens, _) =
        tokens |> Seq.mapFold nextToken  (true, -1, 0)
    parsedTokens

let createParser =
    Parser (fun parsedTokens -> Seq.empty)

let parse (parser: Parser<'T>) tokens =
    let (Parser parseFunc) = parser
    parseFunc tokens
