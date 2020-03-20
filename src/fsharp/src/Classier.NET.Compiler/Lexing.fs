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

let readLines<'T> (src: (unit -> TextReader)) =
    use r1 = src()
    use r2 = src()
    0

//let tokenize<'T> (src, tmap) =
//    let lines = readLines(src)
