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
