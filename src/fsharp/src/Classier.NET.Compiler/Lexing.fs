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
    
// TODO: Need to append chars and determine when a line is formed.
let toFullLine chars isLine =
    match chars with
    | empty when empty |> Seq.isEmpty ->
        String.Empty
    | _ ->
        "T" // TODO: How do we peek the next character and make sure that it isn't a '\n' for when the newline is '\r\n'. First, take until isLine returns true, then takeUntil isLine returns false.

let readLines chars isLine =
    let rec nextLine start =
        seq {
            let line = toFullLine (chars |> Seq.skip(start)) isLine

            match line.Length with
            | 0 ->
                yield! Seq.empty
            | _ ->
                yield line
                yield! nextLine (start + line.Length)
        }

    nextLine 0

// TODO: Check for other newline characters
let isFullLine (line: string) = line.EndsWith("\n")

//let tokenize<'T> (src, tmap) =
//    let lines = readLines(src)
