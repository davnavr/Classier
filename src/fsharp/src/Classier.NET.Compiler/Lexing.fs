// Copyright (c) 2020, David Navarro. All rights reserved.
// Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.

module Classier.NET.Compiler.Lexing

open System
open System.IO
open System.Text.RegularExpressions

type Token<'T> =
    {
        Content: string
        LineNum: int
        LinePos: int
        Type: 'T
    }
    
// TODO: Need to determine when a line is formed.
let readFullLine (chars: seq<char>, start) isLine =
    match chars |> Seq.skip(start) with
    | none when none |> Seq.isEmpty ->
        String.Empty
    | remaining -> "Test"

let readLines chars isLine =
    let rec next start =
        seq {
            let line = readFullLine (chars, start) isLine

            match line.Length with
            | 0 ->
                yield! Seq.empty
            | _ ->
                yield line
                yield! next (start + line.Length)
        }

    next 0

// TODO: Check for other newline characters
let isFullLine (line: string) = line.EndsWith("\n")

//let tokenize<'T> (src, tmap) =
//    let lines = readLines(src)
