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
    using FakeItEasy;
    using Xunit;

    public class TextTokenCollectionTests
    {
        [Theory]
        [InlineData(" public class", TokenType.Whitespace, TokenType.AccessModifier, TokenType.Whitespace, TokenType.Keyword)]
        [InlineData("public class MyClass", TokenType.AccessModifier, TokenType.Whitespace, TokenType.Keyword, TokenType.Whitespace, TokenType.Identifier)]
        [InlineData("() abstract{}", TokenType.OpenParen, TokenType.CloseParen, TokenType.Whitespace, TokenType.Keyword, TokenType.OpenCurlyBracket, TokenType.CloseCurlyBracket)]
        [InlineData("1+2\n/3", TokenType.NumberLiteral, TokenType.Operator, TokenType.NumberLiteral, TokenType.Operator, TokenType.NumberLiteral)]
        [InlineData("#if DEBUG // This is a comment", TokenType.PreprocessorDir, TokenType.Whitespace, TokenType.Identifier, TokenType.Whitespace, TokenType.SingleLineComment)]
        //// TODO: Add test data for other things.
        public void TokensForClassierSourceAreValid(string source, params TokenType[] expectedTokenTypes)
        {
            // Act
            List<Token> tokenList = new TextTokenCollection(() => new StringReader(source)).ToList();

            // Assert
            Assert.Equal(expectedTokenTypes, tokenList.Select(token => token.TokenType));
        }

        [Fact]
        public void NoMatchAlwaysReturnsUnknownToken()
        {
            // Arrange
            string content = "This is a test.";
            ITokenDefinition definition = A.Fake<ITokenDefinition>();
            A.CallTo(() => definition.GetTokenLength(content)).Returns(0);

            // Act
            List<Token> tokenList = new TextTokenCollection(() => new StringReader(content), new ITokenDefinition[] { definition }).ToList();

            // Assert
            Assert.Equal(tokenList.First().ToString(), content);
        }
    }
}
