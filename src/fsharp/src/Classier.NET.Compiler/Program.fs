// Copyright (c) 2020, David Navarro. All rights reserved.
// Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.

module Classier.NET.Compiler.Program

open System

open Classier.NET.Compiler.Lexing

type TokenType =
    /// The token is of an unknown type.
    | Unknown
    /// The token is an access modifier.
    | AccessModifier

let tokenDefs =
    [
        TokenType.AccessModifier, (matchStr "test")
    ] |> Map.ofList

/// <summary>
/// The entry point of the compiler.
/// </summary>
/// <param name="args">
/// The command line arguments.
/// TODO: Describe the arguments in a list here.
/// </param>
[<EntryPoint>]
let main args =
    let tokenizer = createTokenizer (tokenDefs, TokenType.Unknown)
    -1
