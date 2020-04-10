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

open Classier.NET.Compiler.Matching

type Token<'T> = { Type: 'T; Content: string }

/// Turns a sequence of characters into a sequence of tokens.
type Tokenizer<'T> = Tokenizer of (seq<char> -> seq<Token<'T>>)

type TokenDef<'T> = MatchFunc<char, Token<'T>>

let matchAsStr (f: MatchFunc<'Match, 'Result>) =
    f |> mapMatch (fun r -> r.ToString())

/// Matches against the specified character.
let matchChar c =
    let charLabel = sprintf "char '%c'" c
    matchPredicate (fun ch -> c = ch) charLabel
    |> matchAsStr
    |> addFailMsg (sprintf "Error parsing character '%c'." c)

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
    let strLabel = sprintf "string '%s'" str
    str
    |> Seq.map matchChar
    |> matchChain
    |> mapMatch String.Concat
    |> labelMatch strLabel
    |> addFailMsg (sprintf "Error parsing string '%s'." str)

let matchStrSeq (f: MatchFunc<'Match, seq<string>>) =
    f |> mapMatch String.Concat

let matchStrPair (f: MatchFunc<'Match, string * string>) =
    f |> mapMatch (fun (s1, s2) -> s1 + s2)

let matchStrOptional (f: MatchFunc<'Match, string option>) =
    f |> mapMatch (fun optstr ->
        match optstr with
        | Some str -> str
        | None -> String.Empty)

let createTokenizer (definitions: seq<TokenDef<'T>>, defaultVal: 'T) =
    let tokenDefs = definitions |> Seq.cache
    let tokenFrom (item: Item<char>) =
        let results =
            tokenDefs
            |> Seq.map (fun f ->
                let r = evaluateMatch f item
                match r with
                | Success (token, nextItem) ->
                    Some (token, nextItem)
                | Failure _ -> None)
            |> Seq.where (fun r -> r.IsSome)
            |> Seq.map (fun r -> r.Value)
            |> Seq.cache
            
        if results |> Seq.isEmpty then
            None
        else
            let longestToken =
                results
                |> Seq.reduce (fun (t1, item1) (t2, item2) ->
                    if t1.Content.Length >= t2.Content.Length then // TODO: Ensure that the first items have priority over later items if they have the same length.
                        (t1, item1)
                    else
                        (t2, item2))
            Some longestToken

    let unknownFrom (item: Item<char>) = null

    Tokenizer (fun chars ->
        (itemFrom chars, None)
        |>
        Seq.unfold (fun (item, next: Token<'T> option) ->
            match item with
            | Item _ ->
                match next with
                | None ->
                    match tokenFrom item with
                    | Some (token, nextItem) ->
                        Some (token, (nextItem, None))
                    | None -> None // TODO: Emit an unknown token
                | Some token ->
                    Some (token, (item, None))
            | End _ -> None))

let tokenize (tokenizer: Tokenizer<'T>) chars =
    let (Tokenizer tokenizeFunc) = tokenizer
    tokenizeFunc chars
