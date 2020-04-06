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

type Token<'T> =
    {
        Content: string
        Type: 'T
    }

type TokenDef<'T> =
    {
        Match: MatchFunc<char>
        Type: 'T
    }

/// Turns a sequence of characters into a sequence of tokens.
type Tokenizer<'T> = Tokenizer of (seq<char> -> seq<Token<'T>>)

let tokenContent (token: Token<'T>) = token.Content

let tokenType (token: Token<'T>) = token.Type

let createTokenizer (definitions: seq<TokenDef<'T>>, defaultVal: 'T): Tokenizer<'T> =
    let nextMatch (item: Item<char>) =
        let results =
            definitions
            |> Seq.map (fun def -> def.Type, result (def.Match, item))
            |> Seq.filter (fun (_, r) -> isSuccess r)
            |> Seq.cache

        let longestResult (ctype: 'T, citem: Item<char>) (rtype: 'T, r: MatchResult<char>) =
            let current = (ctype, citem)
            match r with
            | Success nextItem ->
                if nextItem.Index > citem.Index then
                    (rtype, nextItem);
                else
                    current
            | _ -> current

        Seq.fold longestResult (defaultVal, item) results

    let rec nextToken (item: Item<char>) =
        let (matchType, matchItem) = nextMatch item
        
        if matchType = defaultVal then
            // TODO: Return unknown until match is found.
            let (Item (c, _, _)) = item
            { Content = string(c); Type = defaultVal }, item.Next
        else
            { Content = String.Concat(selectItems item matchItem); Type = matchType }, matchItem

    Tokenizer (fun chars ->
        seq {
            let mutable item = itemFrom chars

            while not item.ReachedEnd do
                let (token, next) = nextToken item

                yield token
                item <- next
        })

let tokenize (tokenizer: Tokenizer<'T>) chars =
    let (Tokenizer tokenizeFunc) = tokenizer
    tokenizeFunc chars
