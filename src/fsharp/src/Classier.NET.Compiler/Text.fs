// Copyright (c) 2020, David Navarro. All rights reserved.
// Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.

/// <summary>
/// Contains various functions for manipulating strings and characters.
/// </summary>
module Classier.NET.Compiler.Text

open System

let charAsString c =
    let u = sprintf "%04X" (int(c))
    let d = 
        match c with
        | '\u0000' -> "<NUL>"
        | '\n' -> "<LF>"
        | '\r' -> "<CR>"
        | '\t' -> "<HT>"
        | ctrl when ctrl |> Char.IsControl -> "\\u" + u
        | _ -> string(c)
    sprintf("'%s' (U+%s)") d u
