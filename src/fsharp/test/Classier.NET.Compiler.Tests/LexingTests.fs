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
[<Theory>]
let ``Read lines are valid`` content lcount =
    // Arrange
    let r() = new StringReader("Test")

    // Act
    let lines = readLines(r)

    // Assert
    Assert.Equal(lcount, lines |> Seq.length)
    Assert.Equal(content, String.Concat(lines))
