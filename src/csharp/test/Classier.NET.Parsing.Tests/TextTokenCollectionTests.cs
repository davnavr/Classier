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
        [InlineData(" public class", ClassierTokenType.Whitespace, ClassierTokenType.AccessModifier, ClassierTokenType.Whitespace, ClassierTokenType.Keyword)]
        [InlineData("public class MyClass", ClassierTokenType.AccessModifier, ClassierTokenType.Whitespace, ClassierTokenType.Keyword, ClassierTokenType.Whitespace, ClassierTokenType.Identifier)]
        [InlineData("() abstract{}", ClassierTokenType.OpenParen, ClassierTokenType.CloseParen, ClassierTokenType.Whitespace, ClassierTokenType.Keyword, ClassierTokenType.OpenCurlyBracket, ClassierTokenType.CloseCurlyBracket)]
        [InlineData("1+2\n/3", ClassierTokenType.NumberLiteral, ClassierTokenType.Operator, ClassierTokenType.NumberLiteral, ClassierTokenType.Operator, ClassierTokenType.NumberLiteral)]
        [InlineData("#if DEBUG // This is a comment", ClassierTokenType.PreprocessorDir, ClassierTokenType.Whitespace, ClassierTokenType.Identifier, ClassierTokenType.Whitespace, ClassierTokenType.SingleLineComment)]
        //// TODO: Add test data for other things.
        public void TokensForClassierSourceAreValid(string source, params ClassierTokenType[] expectedTokenTypes)
        {
            // Act
            List<Token> tokenList = new TextTokenCollection(() => new StringReader(source), new ClassierTokenCollection()).ToList();

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
