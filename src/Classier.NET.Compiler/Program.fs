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

    /// The token is an access modifier that indicates unrestricted access.
    | AccPublic = 1
    /// The token is an access modifier restricting access to the current library.
    | AccInternal = 2
    /// The token is an access modifier restricting access to the containing type.
    | AccPrivate = 3
    /// The token is a keyword that indicates the declaration of a class.
    | WrdClass = 4
    /// The token is a keyword that indicates the declaration of a field or local variable.
    | WrdLet = 5
    /// The token is a keyword that indicates the declaration of a namespace.
    | WrdNamespace = 6
    /// The token is a modifier.
    | Modifier = 7

    /// The token indicates the start of a multi-line comment.
    | MLCommentStart = 20
    /// The token indicates the end of a multi-line comment.
    | MLCommentEnd = 21
    /// The token is a single-line comment.
    | SLComment = 22

    /// The token is a plus sign.
    | AddOp = 40
    /// The token is a minus sign.
    | SubOp = 41
    /// The token is an asterisk.
    | MulOp = 42
    /// The token is a slash <c>U+002F</c>.
    | DivOp = 43
    /// The token is an equals sign.
    | EqOp = 44
    /// The token indicates the boolean <c>AND</c>.
    | AndOp = 45
    /// The token indicates the boolean <c>OR</c>.
    | OrOp = 46
    /// The token indicates logical negation.
    | NotOp = 47

    /// The token is a string literal.
    | StrLit = 60
    /// The token is a binary literal.
    | BinLit = 61
    /// The token is a hexadecimal (base-16) literal.
    | HexLit = 62
    /// The token is a numeric literal (base-10) with a fractional component.
    | DecLit = 62
    /// The token is an integer (base-10) literal.
    | IntLit = 63
    /// The token indicates a <c>true</c> boolean value.
    | TrueLit = 64
    /// The token indicates a <c>false</c> boolean value.
    | FalseLit = 65

    /// The token is an open paranthesis.
    | LeftParen = 81
    /// The token is an closed paranthesis.
    | RightParen = 82
    /// The token is a period.
    | Period = 83
    /// The token is a comma.
    | Comma = 84

    /// The token is whitespace (a space or tab).
    | Whitespace = 100
    /// The token indicates a new line.
    | NewLine = 101

    /// The token is an identifier.
    | Identifier = 110

/// Tokenizes the source code.
let tokenizer = createTokenizer ([
        { Type = TokenType.AccPublic; Match = matchStr "public" }
        { Type = TokenType.AccInternal; Match = matchStr "internal" }
        { Type = TokenType.AccPrivate; Match = matchStr "private" }
        { Type = TokenType.WrdClass; Match = matchStr "class" }
        { Type = TokenType.WrdLet; Match = matchStr "let" }
        { Type = TokenType.WrdNamespace; Match = matchStr "namespace" }
        { Type = TokenType.Modifier; Match = matchAnyOf ["extends"; "implements"; "mutable"; "virtual"] matchStr }

        { Type = TokenType.MLCommentStart; Match = matchStr "/*" }
        { Type = TokenType.MLCommentEnd; Match = matchStr "*/" }
        { Type = TokenType.SLComment; Match = matchStr "//" }

        { Type = TokenType.AddOp; Match = matchChar '+' }
        { Type = TokenType.SubOp; Match = matchChar '-' }
        { Type = TokenType.MulOp; Match = matchChar '*' }
        { Type = TokenType.DivOp; Match = matchChar '/' }
        { Type = TokenType.EqOp; Match = matchChar '=' }
        { Type = TokenType.AndOp; Match = matchStr "and" }
        { Type = TokenType.OrOp; Match = matchStr "or" }
        { Type = TokenType.NotOp; Match = matchStr "not" }
        
        { Type = TokenType.BinLit; Match = matchChain [matchStr "0"; matchAnyChar ['b'; 'B']; matchAnyChar ['_'; '0'; '1'] |> matchMany] }
        { Type = TokenType.HexLit; Match = matchChain [matchStr "0"; matchAnyChar ['x'; 'X']; matchAny [matchChar '_'; matchCharRange '0' '9'; matchCharRange 'a' 'f'; matchCharRange 'A' 'F'] |> matchMany]}
        { Type = TokenType.IntLit; Match = matchChain [matchCharRange '0' '9'; matchOptional (matchMany (matchAny [matchChar '_'; matchCharRange '0' '9']))]}
        { Type = TokenType.TrueLit; Match = matchStr "true" }
        { Type = TokenType.FalseLit; Match = matchStr "false" }

        { Type = TokenType.LeftParen; Match = matchChar '(' }
        { Type = TokenType.RightParen; Match = matchChar ')' }
        { Type = TokenType.Period; Match = matchChar '.' }
        { Type = TokenType.Comma; Match = matchChar ',' }

        { Type = TokenType.Whitespace; Match = matchAnyChar [' '; '\t'] |> matchMany }
        { Type = TokenType.NewLine; Match = (matchAnyOf ["\r\n"; "\r"; "\n"; "\u0085"; "\u2028"; "\u2029"] matchStr) }

        { Type = TokenType.Identifier; Match = matchChain [matchAny [matchCharRange 'a' 'z'; matchCharRange 'A' 'Z']; matchOptional (matchMany (matchAny [matchCharRange 'a' 'z'; matchCharRange 'A' 'Z'; matchCharRange '0' '9'; matchChar '_']))]}
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
