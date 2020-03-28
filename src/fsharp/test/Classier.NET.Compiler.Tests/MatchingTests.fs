// Copyright (c) 2020, David Navarro. All rights reserved.
// Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.

module Classier.NET.Compiler.MatchingTests

open System
open System.IO

open Xunit

open Classier.NET.Compiler.Collections
open Classier.NET.Compiler.Matching

[<InlineData('T', "Test")>]
[<InlineData('\r', "\r\n")>]
[<Theory>]
let matchCharIsSuccess (expected, text) =
    // Arrange
    let cur = Cursor(text)

    // Act
    let r = result (matchChar expected, cur)

    // Assert
    Assert.NotEqual(cur.Index, (asSuccess r).Index)

[<InlineData('a', "ABC")>]
[<InlineData(' ', "This is testing")>]
[<Theory>]
let matchCharIsFailureForIncorrectChar (expected, text) =
    // Arrange
    let cur = Cursor(text)

    // Act
    let r = result (matchChar expected, cur)

    // Assert
    let msg, fcur = asFailure r
    Assert.Equal(cur, fcur)
    Assert.Contains("got", msg)

[<Fact>]
let matchCharIsFailureForEmptyText =
    // Arrange
    let cur = Cursor(String.Empty)

    // Act
    let r = result (matchChar 'a', cur)

    // Assert
    let msg, fcur = asFailure r
    Assert.Equal(cur, fcur)
    Assert.Contains("end of", msg)

[<InlineData("Test", "Test")>]
[<InlineData("class MyClass", "class MyClass extends")>]
[<Theory>]
let matchStrIsSuccess (expected, text) =
    // Arrange
    let cur = Cursor(text)

    // Act
    let r = result (matchStr expected, cur)

    // Assert
    Assert.NotEqual(cur.Index, (asSuccess r).Index)

[<InlineData("")>]
[<InlineData("hello")>]
[<Theory>]
let matchStrIsSuccessForEmptyString text =
    // Arrange
    let cur = Cursor(text)

    // Act
    let r = result (matchStr String.Empty, cur)

    // Assert
    Assert.Equal(cur, asSuccess r)

[<InlineData("error", "erro")>]
[<InlineData("oops", "")>]
[<Theory>]
let matchStrIsFailureForEndOfText (expected, text) =
    // Arrange
    let cur = Cursor(text)

    // Act
    let r = result (matchStr expected, cur)

    // Assert
    let msg, fcur = asFailure r
    Assert.Equal(cur, fcur)
    Assert.Contains(expected, msg)
    Assert.Contains("end of", msg)

[<InlineData("oh no", "oh\rno")>]
[<InlineData("self", " elf")>]
[<InlineData("abcd", " efgh")>]
[<InlineData("world", "_world")>]
[<Theory>]
let matchStrIsFailureForIncorrect (expected, text) =
    // Arrange
    let cur = Cursor(text)

    // Act
    let r = result (matchStr expected, cur)

    // Assert
    let msg, fcur = asFailure r
    Assert.Equal(cur, fcur)
    Assert.Contains(expected, msg)
    Assert.Contains("got", msg)
