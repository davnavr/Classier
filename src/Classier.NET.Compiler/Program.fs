// Copyright (c) 2020 David Navarro
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

module Classier.NET.Compiler.Program

open System

open Classier.NET.Compiler.Lexing
open Classier.NET.Compiler.Matching

/// Indicates the type of a token.
type TokenType =
    /// The token is of an unknown type.
    | Unknown = 0
    /// The token is an access modifier.
    | AccessModifier = 1
    /// The token is a language keyword.
    | Keyword = 2
    /// The token is whitespace (a space or tab).
    | Whitespace = 3
    /// The token indicates a new line.
    | NewLine = 4

/// Tokenizes the source code.
let tokenizer = createTokenizer ([
        { Type = TokenType.AccessModifier; Match = (matchAnyOf ["public"; "internal"; "private"] matchStr) }
        { Type = TokenType.Keyword; Match = (matchAnyOf ["class"; "using"] matchStr) }
        { Type = TokenType.Whitespace; Match = matchMany (matchAnyOf [' '; '\t'] matchChar) }
        { Type = TokenType.NewLine; Match = (matchAnyOf ["\r\n"; "\r"; "\n"; "\u0085"; "\u2028"; "\u2029"] matchStr) }
    ], TokenType.Unknown)

/// <summary>
/// The entry point of the compiler.
/// </summary>
/// <param name="args">
/// The command line arguments.
/// TODO: Describe the arguments in a list here.
/// </param>
[<EntryPoint>]
let main args =
    -1
