// Copyright (c) 2020, David Navarro. All rights reserved.
// Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.

module Classier.NET.Compiler.LexingTests

open System
open System.IO

open Xunit

open Classier.NET.Compiler
open Classier.NET.Compiler.Lexing
open Classier.NET.Compiler.Matching
open Classier.NET.Compiler.Program

[<InlineData("  \npublic\n", TokenType.Whitespace, TokenType.NewLine)>]
[<Theory>]
let tokensFromStringAreValid (source: string, [<ParamArray>] expectedTypes: TokenType[]) =
    // Act
    let tokens = tokenize Program.tokenizer source

    // Assert
    Assert.Equal(source, tokens |> Seq.map tokenContent |> String.Concat)
    Assert.Equal<TokenType>(expectedTypes |> Seq.ofArray, tokens |> Seq.map tokenType)
    