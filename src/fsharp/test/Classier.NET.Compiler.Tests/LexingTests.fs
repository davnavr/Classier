// Copyright (c) 2020, David Navarro. All rights reserved.
// Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.

module Classier.NET.Compiler.LexingTests

open System
open System.IO

open Xunit

open Classier.NET.Compiler.Lexing

[<InlineData("This is a test.", 1)>]
[<InlineData("One Line\nTwoLine", 2)>]
[<InlineData("Slash R\r\nSlash N", 2)>]
[<InlineData("\n\nTesting, 1 2 3\n", 4)>]
[<InlineData("", 0)>]
[<Theory>]
let readLinesIsValid content lcount =
    // Act
    let lines: seq<string> = readLines content isFullLine

    // Assert
    Assert.Equal(content, String.Concat(lines))
    Assert.Equal(lcount, lines |> Seq.length)
