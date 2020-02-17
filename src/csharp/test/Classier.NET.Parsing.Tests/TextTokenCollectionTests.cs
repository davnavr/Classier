﻿/*
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
    using ClassierToken = Classier.NET.Parsing.Token<Classier.NET.Parsing.ClassierTokenType>;

    public class TextTokenCollectionTests
    {
        [Theory]
        [InlineData(" public class", ClassierTokenType.Whitespace, ClassierTokenType.Keyword, ClassierTokenType.Whitespace, ClassierTokenType.Keyword)]
        [InlineData("public class MyClass", ClassierTokenType.Keyword, ClassierTokenType.Whitespace, ClassierTokenType.Keyword, ClassierTokenType.Whitespace, ClassierTokenType.Identifier)]
        [InlineData("() abstract{}", ClassierTokenType.OpenParen, ClassierTokenType.CloseParen, ClassierTokenType.Whitespace, ClassierTokenType.Keyword, ClassierTokenType.OpenCurlyBracket, ClassierTokenType.CloseCurlyBracket)]
        [InlineData("1+2/3", ClassierTokenType.IntegerLiteral, ClassierTokenType.Operator, ClassierTokenType.IntegerLiteral, ClassierTokenType.Operator, ClassierTokenType.IntegerLiteral)]
        public void TokensForClassierSourceAreValid(string source, params ClassierTokenType[] expectedTokenTypes)
        {
            // Act
            List<ClassierToken> tokenList = new TextTokenCollection<ClassierTokenType>(() => new StringReader(source), new ClassierTokenCollection()).ToList();

            // Assert
            Assert.Equal(expectedTokenTypes, tokenList.Select(token => token.TokenType));
        }

        [Fact]
        public void NoMatchAlwaysReturnsUnknownToken()
        {
            // Arrange
            string content = "This is a test.";
            ITokenDefinition<int> definition = A.Fake<ITokenDefinition<int>>(); // The generic argument does not matter.
            A.CallTo(() => definition.GetTokenLength(content)).Returns(0);

            // Act
            List<Token<int>> tokenList = new TextTokenCollection<int>(() => new StringReader(content), new ITokenDefinition<int>[] { definition }).ToList();

            // Assert
            Assert.IsType<UnknownTokenDefinition<int>>(tokenList.First());
        }
    }
}
