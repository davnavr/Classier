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

type Node<'T> =
    {
        Tokens: seq<Token<'T>>
        Children: seq<Node<'T>>
        Type: 'T
    }

/// Turns a sequence of tokens into a sequence of nodes.
type Parser<'T> = Parser of (seq<Token<'T>> -> seq<Node<'T>>)

// let createParser 

let parse (parser: Parser<'T>) tokens =
    let (Parser parseFunc) = parser
    parseFunc tokens
