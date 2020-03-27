// Copyright (c) 2020, David Navarro. All rights reserved.
// Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.

module Classier.NET.Compiler.LexingTests

open System
open System.IO

open Xunit

open Classier.NET.Compiler.Collections
open Classier.NET.Compiler.Lexing
open Classier.NET.Compiler.Matching

let isSuccess r =
    match r with
    | Success _ -> true
    | Failure _ -> false

[<InlineData('T', "Test", true)>]
[<InlineData('\r', "\r\n", true)>]
[<InlineData('a', "ABC", false)>]
[<InlineData('b', "", false)>]
[<Theory>]
let matchCharIsSuccess (c, text, success) =
    // Arrange
    let (Match m) = matchChar c

    // Act
    let r = m (Cursor(text))

    // Assert
    Assert.Equal(success, isSuccess r)
