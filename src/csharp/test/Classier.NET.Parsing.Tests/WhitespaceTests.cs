/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing.Tests
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    using Xunit;

    public class WhitespaceTests
    {
        [Theory]
        [InlineData("    ", 1)]
        [InlineData("   \t ", 1)]
        [InlineData(" \n", 1)]
        [InlineData("      \n  ", 2)]
        [InlineData("  \r\n  ", 2)]
        [InlineData("    \r\n  \n", 2)]
        [InlineData("   \r\n\n", 2)]
        [InlineData("\n\n\n\n\n", 5)]
        [InlineData("\n\n\r\n\n\n", 5)]
        public void WhitespaceFromTokensIsValid(string text, int tokenCount)
        {
            // Arrange
            var tokens = new TextTokenCollection(() => new StringReader(text));

            // Act
            var ws = new Whitespace(tokens);

            // Assert
            Assert.Equal(tokenCount, ws.Tokens.Count);
            Assert.Equal(text, ws.ToString());
            Assert.All(ws.Tokens, token => Assert.True(token.TokenType == TokenType.Whitespace));
        }
    }
}
