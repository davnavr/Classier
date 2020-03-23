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

let readLine r =
    "Test"

/// <summary>
/// Reads the lines from a <see cref="TextReader"/> and returns them.
/// </summary>
/// <param name="src">Provides the <see cref="TextReader"/> used to read the lines.</param>
/// <returns>A sequence containing the lines, including the newline characters used at the end of each line.</returns>
let readLines src =
    seq {
        use r: #TextReader = src()
        let next _ = readLine (fun() -> r.Read())
        let notEnd _ = r.Peek() >= 0

        yield! Seq.initInfinite next |> Seq.takeWhile notEnd
    }

//let tokenize<'T> (src, tmap) =
//    let lines = readLines(src)
