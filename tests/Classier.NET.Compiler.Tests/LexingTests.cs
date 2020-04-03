/*
 * Copyright (c) 2020, David Navarro.
 */

namespace Classier.NET.Compiler
{
    using System.Linq;
    using Xunit;
    using static Classier.NET.Compiler.Lexing;
    using static Classier.NET.Compiler.Program;

    public class LexingTests
    {
        [InlineData("  \npublic", TokenType.Whitespace, TokenType.NewLine, TokenType.AccessModifier)]
        [Theory]
        public void TokensFromStringAreValid(string source, params TokenType[] expectedTypes)
        {
            // Act
            var tokens = tokenize(Program.tokenizer, source);

            // Assert
            Assert.Equal(source, string.Concat(tokens.Select(token => token.Content)));
            Assert.Equal(expectedTypes, tokens.Select(token => token.Type));
        }
    }
}
