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
    using System.Linq;
    using Xunit;
    using static Classier.NET.Compiler.Grammar;
    using static Classier.NET.Compiler.Lexing;
    using static Classier.NET.Compiler.Matching;
    using static Classier.NET.Compiler.Parsing;

    public class ParsingTests
    {
#pragma warning disable IDE0002 // Name can be simplified

#if false
        [InlineData("I am one line", 1, new[] { 0, 1, 2, 4, 5, 8, 9 })]
        [InlineData("This file\nis licensed\nunder some\nlicense", 4, new[] { 0, 4, 5, 9, 0, 2, 3, 11, 0, 5, 6, 10, 0 })]
        [InlineData("This\nshould\nbe three\n", 3, new[] { 0, 4, 0, 6, 0, 2, 3, 8 })]
        [InlineData("Works\nfor\rall\r\nnewline\u0085chars\u2028that\u2029exist", 7, new[] { 0, 5, 0, 3, 0, 3, 0, 7, 0, 5, 0, 4, 0 })]
        [Theory]
        public void LineInfoProducesCorrectLineNumbersAndPositionsForTokens(string content, int expectedLineCount, int[] expectedTokenPositions)
        {
            // Arrange
            var newline = new InteropFunc<Token<TokenType>, bool>(tokenIsNewline);
            var tokens = tokenize(Grammar.tokenizer, content);

            // Act
            var parsedTokens = lineInfo(tokens, newline);

            // Assert
            Assert.Equal(expectedLineCount, parsedTokens.Last().Line + 1);
            Assert.Equal(expectedTokenPositions, parsedTokens.Select(token => token.Pos));
        }
#endif

#pragma warning restore IDE0002 // Name can be simplified
    }
}
