/*
 * Copyright (c) 2020 David Navarro
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

namespace Classier.NET.Compiler
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using Xunit;
    using static Classier.NET.Compiler.Grammar;
    using static Classier.NET.Compiler.Lexing;
    using static Classier.NET.Compiler.Matching;
    using static Classier.NET.Compiler.Parsing;

    public class LexingTests
    {
#pragma warning disable IDE0002 // Name can be simplified

        [InlineData("  \npublic", 2, TokenType.Whitespace, TokenType.NewLine, TokenType.AccPublic)]
        [InlineData("0b1101_0110+0xABC1_EF39", 1, TokenType.BinLit, TokenType.AddOp, TokenType.HexLit)]
        [InlineData("1600\t // My comment\r\n", 1, TokenType.IntLit, TokenType.Whitespace, TokenType.SLComment, TokenType.NewLine)]
        [InlineData("/*\u0001*/", 1, TokenType.MLCommentStart, TokenType.Unknown, TokenType.MLCommentEnd)]
        [InlineData("myVariable.myMethod(true)", 1, TokenType.Identifier, TokenType.Period, TokenType.Identifier, TokenType.LeftParen, TokenType.TrueLit, TokenType.RightParen)]
        [InlineData("0B010101-0XFFAB", 1, TokenType.BinLit, TokenType.SubOp, TokenType.HexLit)]
        [InlineData("private\r\nclass", 2, TokenType.AccPrivate, TokenType.NewLine, TokenType.WrdClass)]
        [InlineData("one\r\n\r\ntwo\n\nthree", 5, TokenType.Identifier, TokenType.NewLine, TokenType.NewLine, TokenType.Identifier, TokenType.NewLine, TokenType.NewLine, TokenType.Identifier)]
        [InlineData("\u0085\u0002\u0003\u0004extends", 2, TokenType.NewLine, TokenType.Unknown, TokenType.Modifier)]
        [InlineData("let myVar=\"string cheese is ok\"", 1, TokenType.WrdLet, TokenType.Whitespace, TokenType.Identifier, TokenType.EqOp, TokenType.StrLit)]
        [InlineData("\"no_newline\nin_string\"", 2, TokenType.Unknown, TokenType.Identifier, TokenType.NewLine, TokenType.Identifier, TokenType.Unknown)]
        [InlineData("valid__\r\u00EF\u00BB\u00BF", 2, TokenType.Identifier, TokenType.NewLine, TokenType.Unknown)]
        [InlineData("0x____ 0B_____", 1, TokenType.IntLit, TokenType.Identifier, TokenType.Whitespace, TokenType.IntLit, TokenType.Identifier)]
        [InlineData("_____ 0b1____ 0XE__", 1, TokenType.Unknown, TokenType.Whitespace, TokenType.BinLit, TokenType.Whitespace, TokenType.HexLit)]
        [InlineData("// One comment\n// Two comment", 2, TokenType.SLComment, TokenType.NewLine, TokenType.SLComment)]
        [InlineData("class\r\n{\r\n}", 3, TokenType.WrdClass, TokenType.NewLine, TokenType.LCBracket, TokenType.NewLine, TokenType.RCBracket)]
        [Theory]
        public void TokensFromStringAreValid(string source, int expectedLineCount, params TokenType[] expectedTypes)
        {
            // Act
            var tokens = tokenize(Grammar.tokenizer, source);

            // Assert
            Assert.Equal(source, tokens.SelectMany(token => token.Content));
            Assert.Equal(expectedTypes, tokens.Select(token => token.Type));
            Assert.Equal(expectedLineCount - 1, tokens.Last().LineNum);
        }

#pragma warning restore IDE0002 // Name can be simplified
    }
}
