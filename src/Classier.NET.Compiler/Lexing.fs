// Copyright (c) 2020, David Navarro. All rights reserved.
// Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.

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

type Tokenizer<'T> = Tokenizer of (seq<char> -> seq<Token<'T>>)

let tokenContent (token: Token<'T>) = token.Content

let tokenType (token: Token<'T>) = token.Type

let createTokenizer (definitions: seq<TokenDef<'T>>, defaultVal: 'T): Tokenizer<'T> =
    let nextToken (item: Item<char>) =
        // TODO: Need to find a way to append char to the unknown token until a definition is found.
        let results =
            definitions
            |> Seq.map (fun def -> def.Type, result (def.Match, item))
            |> Seq.filter (fun (_, r) -> isSuccess r)
            |> Seq.cache
        
        // TODO: How do we check if the results have a success while also sorting it by the longest success efficiently?

        let tokenMatch =
            (fun x1 x2 -> x1)
            |> Seq.fold

        { Content = "Test"; Type = defaultVal }, nextItem item

    Tokenizer (fun chars ->
        seq {
            let mutable item = itemFrom chars
            
            while not (itemIsEnd item) do
                let (token, next) = nextToken item
                yield token
                item <- next
        })

let tokenize (tokenizer: Tokenizer<'T>) chars =
    let (Tokenizer tokenizeFunc) = tokenizer
    tokenizeFunc chars
