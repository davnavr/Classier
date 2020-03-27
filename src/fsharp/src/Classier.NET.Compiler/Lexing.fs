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

/// <summary>
/// Matches against the specified character.
/// </summary>
/// <param name="char">The character to match.</param>
let matchChar char: MatchFunc<char> =
    let failMsg r = sprintf "Expected character %c, but %s" char r
    Match (fun cur ->
        match cur.Item with
        | Item act ->
            if char = act then
                Success cur.Next
            else
                Failure (failMsg (sprintf "got %c instead" act), cur)
        | End -> Failure (failMsg "the end of the text was reached instead.", cur))

/// <summary>
/// Matches against a sequence of characters.
/// </summary>
/// <param name="str">The expected sequence of characters.</param>
let matchStr str: MatchFunc<char> =
    match str with
    | empty when empty |> Seq.isEmpty ->
        Match (fun cur -> Success cur)
    | _ ->
        Match (fun cur ->
            let r = result (str |> Seq.map matchChar |> Seq.reduce andThen, cur)
            match r with
            | Success _ -> r
            | Failure (msg, c) -> Failure (sprintf "Cannot parse '%s'. %s" str msg, c))

let createTokenizer<'T when 'T : comparison> (definitions: seq<TokenDef<'T>>, defaultVal: 'T): Tokenizer<'T> =
    let results cur =
        definitions
            |> Seq.map (fun def -> def.Type, result (def.Match, cur))
            |> Seq.filter (fun (_, result) ->
                match result with
                | Success _ -> true
                | Failure _ -> false)
    let nextToken (cur: Cursor<char>) =
        // TODO: Find longest token
        let tokenDef = results cur // |> Seq.fold
        
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
