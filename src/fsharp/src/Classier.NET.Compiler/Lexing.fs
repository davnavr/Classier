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
        LineNum: int
        LinePos: int
        Type: 'T
    }

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
let matchStr (str: seq<char>): MatchFunc<string> =
    match str with
    | empty when empty |> Seq.isEmpty ->
        Match (fun cur -> Success cur)
    | _ ->
        Match (fun cur ->
            Failure ("Not implemented", cur))

// let tokenize<'T> chars tmap =
