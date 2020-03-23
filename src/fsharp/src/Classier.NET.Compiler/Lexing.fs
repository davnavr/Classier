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
    
// TODO: Need to read characters and store them, maybe use recursion here?
let readSingleLine read isLine =
    let appendChar line =
        let c = read()
        let singleLine() =
            let fullLine = line + string(char(c))
            if isLine(fullLine) then fullLine else "Do the recursive call here"

        match c with
        | -1 -> String.Empty
        | _ -> singleLine()

    appendChar String.Empty

let readLines read isLine =
    let next _ = readSingleLine read isLine

    Seq.initInfinite next |> Seq.takeWhile (fun str -> str.Length > 0)

//let tokenize<'T> (src, tmap) =
//    let lines = readLines(src)
