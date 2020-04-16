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

/// Contains types and functions for turning a sequence of characters into a sequence of tokens.
module Classier.NET.Compiler.Lexing

open System

open Classier.NET.Compiler.Item
open Classier.NET.Compiler.Matching

/// Turns a sequence of characters into a sequence of tokens.
type Tokenizer<'Token> = Tokenizer of (seq<char> -> seq<'Token>)

/// Matches against the specified character.
let matchChar c =
    (fun ch -> c = ch)
    |> matchPredicate (sprintf "char '%c'" c)

/// Matches against any of the specified characters.
let matchAnyChar chars = matchAnyOf chars matchChar

/// Matches against the characters in the specified range, inclusive.
let rec matchCharRange (c1, c2) =
    if c1 > c2 then
        matchCharRange (c2, c1)
    else
        matchAnyChar (seq { c1..c2 })

/// Matches against the specified string.
let matchStr str =
    str
    |> Seq.map matchChar
    |> matchChain
    |> labelMatch (sprintf "string '%s'" str)

let tokenizerFrom (definitions: seq<'Definition>) (definitionMap: 'Definition -> MatchFunc<char>) (generator: 'Definition option -> seq<char> -> 'Token) =
    let tokenFrom item =
        let results = None

        None

    let unknownFrom startItem = invalidOp "Not yet implemented."

    Tokenizer (fun chars ->
        if Seq.isEmpty definitions then
            generator None chars |> Seq.singleton
        else
            (Item.fromSeq chars, None)
            |> Seq.unfold (fun (item, next: 'Token option) ->
                let nextVal token nextItem nextToken =
                    Some (token, (nextItem, nextToken))

                // Check if we have to emit an unknown token.
                match next with
                | None ->
                    match item with
                    | Some currentItem ->
                        match tokenFrom item with
                        | Some (token, nextItem) ->
                            nextVal token nextItem None
                        | None ->
                            let (unknownToken, nextItem, nextToken) = unknownFrom item
                            nextVal unknownToken nextItem nextToken
                    | None -> None
                | Some token ->
                    nextVal token item None))

let tokenize (tokenizer: Tokenizer<'Token>) chars =
    let (Tokenizer tokenizeFunc) = tokenizer
    tokenizeFunc chars
