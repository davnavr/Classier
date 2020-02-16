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

    public class TextTokenCollectionTests
    {
        [Theory]
        [InlineData("1 + 1")]
        [InlineData("1 + 1 * 3 - 4 / 5")]
        public void TokensForMathExpressionAreValid(string statement)
        {
            // Arrange
            List<ITokenDefinition> definitions = null;

            // Act
            List<Token> tokenList = new TextTokenCollection(() => new StringReader(statement), definitions).ToList();

            // Assert
            throw new NotImplementedException();
        }
    }
}
