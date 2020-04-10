﻿// Copyright (c) 2020 David Navarro
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

module rec Classier.NET.Compiler.Grammar

#nowarn "40"

open Classier.NET.Compiler.Lexing
open Classier.NET.Compiler.Matching
open Classier.NET.Compiler.Parsing

/// Indicates the type of a token.
type TokenType =
    | ``a-fA-F0-9`` = -3
    /// Matches any character of the English alphabet, regardless of case.
    | ``a-zA-Z`` = -2
    /// Matches any numeric character.
    | ``0-9`` = -1
    /// The token is of an unknown type.
    | Unknown = 0
    
    /// The token indicates the start of a multi-line comment.
    | MLCommentStart = 1
    /// The token indicates the end of a multi-line comment.
    | MLCommentEnd = 2
    /// The token is a single-line comment.
    | SLComment = 3

    /// The token is an access modifier that indicates unrestricted access.
    | AccPublic = 21
    /// The token is an access modifier restricting access to the current library.
    | AccInternal = 22
    /// The token is an access modifier restricting access to the containing type.
    | AccPrivate = 23
    /// The token is a keyword that indicates the declaration of a class.
    | WrdClass = 24
    /// The token is a keyword that indicates the declaration of a field or local variable.
    | WrdLet = 25
    /// The token is a keyword that indicates the declaration of a namespace.
    | WrdNamespace = 26
    /// The token is a keyword that allows the use of types without their fully qualified names.
    | WrdUse = 27
    /// The token is a modifier.
    | Modifier = 28

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
    /// The token is a left curly bracket <c>U+007B</c>.
    | LCBracket = 48
    /// The token is a right curly bracket <c>U+007D</c>.
    | RCBracket = 49

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

let tokenIsNewline (token: Token<TokenType>) = token.Type = TokenType.NewLine

let matchTokenType t = lazy (tokenizerDefs.Item t) |> matchLazy

let tokenizerDefs: Map<TokenType, MatchFunc<char, string>> =
    [
        TokenType.``a-fA-F0-9``, matchAny [matchTokenType TokenType.``0-9``; matchCharRange ('a', 'f'); matchCharRange ('A', 'F')];
        TokenType.``a-zA-Z``, matchCharRange ('A', 'Z') |> orMatch (matchCharRange ('a', 'z'));
        TokenType.``0-9``, matchCharRange ('0', '9');

        TokenType.MLCommentStart, matchStr "/*";
        TokenType.MLCommentEnd, matchStr "*/";
        //TokenType.SLComment, matchChain [matchStr "//"; orMatch (matchTokenType TokenType.NewLine |> matchUntil) (matchUntilEnd)];

        TokenType.AccPublic, matchStr "public";
        TokenType.AccInternal, matchStr "internal";
        TokenType.AccPrivate, matchStr "private";
        TokenType.WrdClass, matchStr "class";
        TokenType.WrdLet, matchStr "let";
        TokenType.WrdNamespace, matchStr "namespace";
        TokenType.WrdUse, matchStr "use";
        TokenType.Modifier, matchAnyOf ["extends"; "implements"; "mutable"; "virtual"] matchStr;

        TokenType.AddOp, matchChar '+';
        TokenType.SubOp, matchChar '-';
        TokenType.MulOp, matchChar '*';
        TokenType.DivOp, matchChar '/';
        TokenType.EqOp, matchChar '=';
        TokenType.AndOp, matchStr "and";
        TokenType.OrOp, matchStr "or";
        TokenType.NotOp, matchStr "not";
        TokenType.LCBracket, matchChar '{';
        TokenType.RCBracket, matchChar '}';

        //TokenType.StrLit, andMatch (matchChar '"') (matchTo (matchChar '"') |> matchWithout (matchTokenType TokenType.NewLine));
        TokenType.BinLit, matchChain [matchChar '0'; matchAnyChar ['b'; 'B']; matchAnyChar ['0'; '1']; matchOptional (matchAnyChar ['_'; '0'; '1'] |> matchMany |> matchStrSeq) |> matchStrOptional] |> matchStrSeq;
        TokenType.HexLit, matchChain [matchChar '0'; matchAnyChar ['x'; 'X']; matchTokenType TokenType.``a-fA-F0-9``; matchTokenType TokenType.``a-fA-F0-9`` |> orMatch (matchChar '_') |> matchMany |> matchStrSeq] |> matchStrSeq;
        TokenType.IntLit, matchChain [matchTokenType TokenType.``0-9``; matchOptional (matchAny [matchChar '_'; matchTokenType TokenType.``0-9``] |> matchMany |> matchStrSeq) |> matchStrOptional] |> matchStrSeq;
        TokenType.TrueLit, matchStr "true";
        TokenType.FalseLit, matchStr "false";

        TokenType.LeftParen, matchChar '(';
        TokenType.RightParen, matchChar ')';
        TokenType.Period, matchChar '.';
        TokenType.Comma, matchChar ',';

        TokenType.Whitespace, matchAnyChar [' '; '\t'] |> matchMany |> matchStrSeq;
        TokenType.NewLine, matchAnyOf ["\r\n"; "\r"; "\n"; "\u0085"; "\u2028"; "\u2029"] matchStr;
        
        TokenType.Identifier, matchChain [matchTokenType TokenType.``a-zA-Z``; matchOptional (matchMany (matchAny [matchTokenType TokenType.``a-zA-Z``; matchTokenType TokenType.``0-9``; matchChar '_']) |> matchStrSeq) |> matchStrOptional] |> matchStrSeq;
    ] |> Map.ofList

let tokenizer: Tokenizer<TokenType> =
    createTokenizer(
        tokenizerDefs
            |> Map.toSeq
            |> Seq.filter (fun (t, _) -> t > TokenType.Unknown)
            |> Seq.map (fun (t, f) ->
                f
                |> mapMatch (fun str -> { Type = t; Content = str })
                |> labelMatch (sprintf "%A (%s)" t f.Label)),
        TokenType.Unknown)
