// Copyright (c) 2020, David Navarro. All rights reserved.
// Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.

/// Contains types and functions for turning a sequence of characters into a sequence of tokens.
module Classier.NET.Compiler.Lexing

open System

open Classier.NET.Compiler.Collections
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

let createTokenizer<'T when 'T : comparison> (definitions: seq<TokenDef<'T>>, defaultVal: 'T): Tokenizer<'T> =
    let results cur =
        definitions
        |> Seq.map (fun def -> def.Type, result (def.Match, cur))
        |> Seq.filter (fun (_, result) -> isSuccess result)
    let nextToken (cur: Cursor<char>) =
        // TODO: Find longest token
        let matches = results cur // TODO: Need to find a way to append char to the unknown token until a definition is found.
        //let tokenDef =  // |> Seq.fold

        { Content = "Test"; Type = defaultVal }, cur.Next 

    Tokenizer (fun chars ->
        seq {
            let mutable cur = Cursor(chars)
            
            while not cur.ReachedEnd do
                let (ntoken, ncur) = nextToken cur
                yield ntoken
                cur <- ncur
        })

let tokenize<'T> (tokenizer: Tokenizer<'T>) chars =
    let (Tokenizer tokenizeFunc) = tokenizer
    tokenizeFunc chars
