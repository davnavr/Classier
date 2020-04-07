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
    using System.Linq;
    using Xunit;
    using static Classier.NET.Compiler.Lexing;
    using static Classier.NET.Compiler.Matching;
    using static Classier.NET.Compiler.Program;

#pragma warning disable IDE0002 // Name can be simplified

    public class LexingTests
    {
        [InlineData("  \npublic", TokenType.Whitespace, TokenType.NewLine, TokenType.AccPublic)]
        [InlineData("0b1101_0110+0xABC1_EF39", TokenType.BinLit, TokenType.AddOp, TokenType.HexLit)]
        [InlineData("1600\t // My comment", TokenType.IntLit, TokenType.Whitespace, TokenType.SLComment, TokenType.Whitespace, TokenType.Identifier, TokenType.Whitespace, TokenType.Identifier)]
        [InlineData("/*\u0001*/", TokenType.MLCommentStart, TokenType.Unknown, TokenType.MLCommentEnd)]
        [InlineData("myVariable.myMethod(true)", TokenType.Identifier, TokenType.Period, TokenType.Identifier, TokenType.LeftParen, TokenType.TrueLit, TokenType.RightParen)]
        [InlineData("0B010101-0XFFAB", TokenType.BinLit, TokenType.SubOp, TokenType.HexLit)]
        [InlineData("private\r\nclass", TokenType.AccPrivate, TokenType.NewLine, TokenType.WrdClass)]
        [InlineData("one\r\n\r\ntwo\n\nthree", TokenType.Identifier, TokenType.NewLine, TokenType.NewLine, TokenType.Identifier, TokenType.NewLine, TokenType.NewLine, TokenType.Identifier)]
        [InlineData("\u0085\u0002\u0003\u0004extends", TokenType.NewLine, TokenType.Unknown, TokenType.Modifier)]
        [InlineData("let myVar=\"string cheese is ok\"", TokenType.WrdLet, TokenType.Whitespace, TokenType.Identifier, TokenType.EqOp, TokenType.StrLit)]
        [InlineData("\"no_newline\nin_string\"", TokenType.Unknown, TokenType.Identifier, TokenType.NewLine, TokenType.Identifier, TokenType.Unknown)]
        [Theory]
        public void TokensFromStringAreValid(string source, params TokenType[] expectedTypes)
        {
            // Act
            var tokens = tokenize(Program.tokenizer, source);

            // Assert
            Assert.Equal(source, string.Concat(tokens.Select(token => token.Content)));
            Assert.Equal(expectedTypes, tokens.Select(token => token.Type));
        }

        [Fact]
        public void MatchTokenIsSuccessForMatchingType()
        {
            // Arrange
            var tokens = new[] { new Token<TokenType>("this can be anything", TokenType.Whitespace) };

            // Act
            var success = (MatchResult<Token<TokenType>>.Success)result(matchToken(TokenType.Whitespace), itemFrom(tokens));

            // Assert
            Assert.Equal(1, success.Item.Index);
        }

        [Fact]
        public void MatchTokenIsFailureForIncorrectType()
        {
            // Arrange
            var actual = TokenType.Unknown;
            var expected = TokenType.WrdClass;
            var tokens = new[] { new Token<TokenType>("about:blank", actual) };

            // Act
            var failure = (MatchResult<Token<TokenType>>.Failure)result(matchToken(expected), itemFrom(tokens));

            // Assert
            Assert.Equal(0, failure.Item2.Index);
            Assert.Contains(actual.ToString(), failure.Item1);
            Assert.Contains(expected.ToString(), failure.Item1);
        }

        [Fact]
        public void MatchTokenIsFailureForEndOfSequence()
        {
            // Arrange
            var expected = TokenType.WrdClass;
            var tokens = new Token<TokenType>[0];

            // Act
            var failure = (MatchResult<Token<TokenType>>.Failure)result(matchToken(expected), itemFrom(tokens));

            // Assert
            Assert.Equal(0, failure.Item2.Index);
            Assert.Contains("end of the token sequence", failure.Item1);
            Assert.Contains(expected.ToString(), failure.Item1);
        }
    }

#pragma warning restore IDE0002 // Name can be simplified
}
