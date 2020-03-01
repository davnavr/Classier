/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing.Tests
{
    using System.IO;
    using Xunit;

    public class MultiLineCommentTests
    {
        [Theory]
        [InlineData("/* This is my comment */", 11)]
        [InlineData("/*Hello */    ", 4)]
        [InlineData("/**/", 2)]
        [InlineData("/*/*Test*/", 4)]
        public void MultiLineCommentNodeFromStringIsValid(string content, int tokenCount)
        {
            // Act
            var comment = new MultiLineCommentNode(new TextTokenCollection(() => new StringReader(content)));

            // Assert
            Assert.NotEmpty(comment.ToString());
            Assert.StartsWith(content.ToString(), content);
            Assert.Equal("/*", comment.StartToken.ToString());
            Assert.Equal("*/", comment.EndToken.ToString());
            Assert.Equal(tokenCount, comment.Tokens.Count);
        }
    }
}
