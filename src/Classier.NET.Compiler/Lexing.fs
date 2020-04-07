﻿// Copyright (c) 2020 David Navarro
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
            None, matchItem.Next
        else
            Some { Content = String.Concat(selectItems item matchItem); Type = matchType }, matchItem

    Tokenizer (fun chars ->
        seq {
            let mutable item = itemFrom chars
            let mutable unknown = item

            while not item.ReachedEnd do
                let (token, next) = nextToken item

                match token with
                | Some t ->
                    if not (item.Index = unknown.Index) then
                        yield { Content = String.Concat(selectItems unknown item); Type = defaultVal }

                    yield t
                    unknown <- next
                | None ->
                    ()
                
                item <- next
        })

let tokenize (tokenizer: Tokenizer<'T>) chars =
    let (Tokenizer tokenizeFunc) = tokenizer
    tokenizeFunc chars

let matchToken (t: 'T) =
    Match (fun (item: Item<Token<'T>>) ->
        let failMsg r = sprintf "Expected a token of type '%s', but %s" (t.ToString()) r
        match item with
        | Item (token, _, next) ->
            if token.Type = t then
                Success next.Value
            else
                Failure (sprintf "got a '%s' token instead." (token.Type.ToString()) |> failMsg, item)
        | End _ ->
            Failure (failMsg "the end of the token sequence was reached instead.", item))
