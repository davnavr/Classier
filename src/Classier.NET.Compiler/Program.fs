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
    | AccModifier = 1
    /// The token is a language keyword.
    | Keyword = 2

    /// The token is a plus sign.
    | AddOp = 50
    /// The token is a minus sign.
    | SubOp = 51
    /// The token is an asterisk.
    | MulOp = 52
    /// The token is a slash <c>U+002F</c>.
    | DivOp = 53
    /// The token indicates the logical <c>AND</c>.
    | AndOp = 54
    /// The token indicates the logical <c>OR</c>.
    | OrOp = 55
    /// The token indicates the logical <c>XOR</c>.
    | XorOp = 55

    /// The token is a multi-line comment.
    | MLComment = 10
    /// The token is a single-line comment.
    | SLComment = 11

    /// The token is a string literal.
    | StrLit = 30
    /// The token is a binary literal.
    | BinLit = 31
    /// The token is a hexadecimal (base-16) literal.
    | HexLit = 33
    /// The token is a numeric literal (base-10) with a fractional component.
    | DecLit = 32
    /// The token is an integer (base-10) literal.
    | IntLit = 34
    /// The token indicates a <c>true</c> boolean value.
    | TrueLit = 35
    /// The token indicates a <c>false</c> boolean value.
    | FalseLit = 36

    // The token is an open paranthesis.
    | LeftParen = 41
    // The token is an closed paranthesis.
    | RightParen = 42
    // The token is a period.
    | Period = 43

    /// The token is whitespace (a space or tab).
    | Whitespace = 98
    /// The token indicates a new line.
    | NewLine = 99

    /// The token is an identifier.
    | Identifier = 100

/// Tokenizes the source code.
let tokenizer = createTokenizer ([
        { Type = TokenType.AccModifier; Match = (matchAnyOf ["public"; "internal"; "private"] matchStr) }
        { Type = TokenType.Keyword; Match = (matchAnyOf ["class"] matchStr) }

        { Type = TokenType.AddOp; Match = matchChar '+' }
        { Type = TokenType.SubOp; Match = matchChar '-' }
        { Type = TokenType.MulOp; Match = matchChar '*' }
        { Type = TokenType.DivOp; Match = matchChar '/' }

        { Type = TokenType.LeftParen; Match = matchChar '(' }
        { Type = TokenType.RightParen; Match = matchChar ')' }
        { Type = TokenType.Period; Match = matchChar '.' }

        { Type = TokenType.Whitespace; Match = matchMany (matchAnyOf [' '; '\t'] matchChar) }
        { Type = TokenType.NewLine; Match = (matchAnyOf ["\r\n"; "\r"; "\n"; "\u0085"; "\u2028"; "\u2029"] matchStr) }

        { Type = TokenType.Identifier; Match = failure "Not yet implemented."}

        { Type = TokenType.AndOp; Match = matchStr "and" }
        { Type = TokenType.OrOp; Match = matchStr "or" }
        { Type = TokenType.XorOp; Match = matchStr "xor" }

        { Type = TokenType.TrueLit; Match = matchStr "true" }
        { Type = TokenType.FalseLit; Match = matchStr "false" }
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
