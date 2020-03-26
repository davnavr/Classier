// Copyright (c) 2020, David Navarro. All rights reserved.
// Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.

module Classier.NET.Compiler.Lexing

open Classier.NET.Compiler.Collections
open Classier.NET.Compiler.Text

type Token<'T> =
    {
        Content: string
        LineNum: int
        LinePos: int
        Type: 'T
    }

type MatchResult =
    | Success of Cursor<char>
    | Failure of string

let matchChar (exp: char) (cur: Cursor<char>) =
    let failMsg = sprintf "Expected character %s, but " (charAsString(exp))

    match cur.Item with
    | Item act ->
        if exp = act then
            Success cur.Next
        else
            Failure (failMsg + sprintf "got %s instead" (charAsString act))
    | End -> Failure (failMsg + "the end of the text was reached instead.")

    

let matchSeq (exp: seq<char>) cur = Failure "Not implemented."

// let tokenize<'T> chars tmap =
