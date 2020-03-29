// Copyright (c) 2020, David Navarro. All rights reserved.
// Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.

module Classier.NET.Compiler.MatchingTests

open System
open System.IO

open Xunit

open Classier.NET.Compiler.Matching

[<InlineData('T', "Test")>]
[<InlineData('\r', "\r\n")>]
[<Theory>]
let matchCharIsSuccess (expected, text) =
    // Arrange
    let item = itemFrom text

    // Act
    let r = result (matchChar expected, item)

    // Assert
    Assert.NotEqual(itemIndex item, asSuccess r |> itemIndex)

[<InlineData('a', "ABC")>]
[<InlineData(' ', "This is testing")>]
[<Theory>]
let matchCharIsFailureForIncorrectChar (expected, text) =
    // Arrange
    let item = itemFrom text

    // Act
    let r = result (matchChar expected, item)

    // Assert
    let msg, fitem = asFailure r
    Assert.Equal(itemIndex item, itemIndex fitem)
    Assert.Contains("got", msg)

[<Fact>]
let matchCharIsFailureForEmptyText =
    // Arrange
    let item = itemFrom "airline"

    // Act
    let r = result (matchChar 'a', item)

    // Assert
    let msg, fitem = asFailure r
    Assert.Equal(itemIndex item, itemIndex fitem)
    Assert.Contains("end of", msg)

[<InlineData("Test", "Test")>]
[<InlineData("class MyClass", "class MyClass extends")>]
[<Theory>]
let matchStrIsSuccess (expected, text) =
    // Arrange
    let item = itemFrom text

    // Act
    let r = result (matchStr expected, item)

    // Assert
    Assert.NotEqual(itemIndex item, asSuccess r |> itemIndex)

[<InlineData("")>]
[<InlineData("hello")>]
[<Theory>]
let matchStrIsSuccessForEmptyString text =
    // Arrange
    let item = itemFrom text

    // Act
    let r = result (matchStr String.Empty, item)

    // Assert
    Assert.Equal(itemIndex item, asSuccess r |> itemIndex)

[<InlineData("error", "erro")>]
[<InlineData("oops", "")>]
[<Theory>]
let matchStrIsFailureForEndOfText (expected, text) =
    // Arrange
    let item = itemFrom text

    // Act
    let r = result (matchStr expected, item)

    // Assert
    let msg, fitem = asFailure r
    Assert.Equal(itemIndex item, itemIndex fitem)
    Assert.Contains(expected, msg)
    Assert.Contains("end of", msg)

[<InlineData("oh no", "oh\rno")>]
[<InlineData("self", " elf")>]
[<InlineData("abcd", " efgh")>]
[<InlineData("world", "_world")>]
[<Theory>]
let matchStrIsFailureForIncorrect (expected, text) =
    // Arrange
    let item = itemFrom text

    // Act
    let r = result (matchStr expected, item)

    // Assert
    let msg, fitem = asFailure r
    Assert.Equal(itemIndex item, itemIndex fitem)
    Assert.Contains(expected, msg)
    Assert.Contains("got", msg)
