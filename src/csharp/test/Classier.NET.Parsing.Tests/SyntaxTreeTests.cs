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

    public class SyntaxTreeTests
    {
        [Theory]
        [InlineData(@"// Some copyright info

using system.console;

public class MyClass
{
    public void myMethod()
    {
         console.writeLine(""Test"");
    }
}")]
        public void NodesForStringAreCorrect(string text)
        {
            // Arrange
            var tokens = new TextTokenCollection(() => new StringReader(text));

            // Act
            var tree = new SyntaxTree(tokens);

            // Assert
            throw new NotImplementedException();
        }
    }
}
