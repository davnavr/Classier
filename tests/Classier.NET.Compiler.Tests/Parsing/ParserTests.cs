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

namespace Classier.NET.Compiler.Parsing
{
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using FParsec;
    using Xunit;
    using static Classier.NET.Compiler.Parsing.Grammar;
    using static FParsec.CharParsers;

    public class ParserTests
    {
        [InlineData("MultipleClasses.txt")]
        [InlineData("MyAbstractClass1.txt", new[] { "java", "lang" }, new[] { "java", "util" })]
        [InlineData("MyModule1.txt", new[] { "system", "reflection" })]
        [Theory]
        public void ParserCorrectlySetsUsings(string name, params string[][] usings)
        {
            // Arrange
            using var stream = new EmbeddedSourceFile(name).GetStream();

            // Act
            var result = new SuccessResult(parser, stream, name, Encoding.UTF8).Result;

            // Assert
            Assert.Equal(usings, result.Usings.Cast<IEnumerable<string>>());
        }

        [InlineData("MyClass1.txt", new string[0])]
        [InlineData("MyGenericClass1.txt", new[] { "some", "name", "collections" })]
        [Theory]
        public void ParserCorrectlySetsNamespace(string name, string[] namespaceName)
        {
            // Arrange
            using var stream = new EmbeddedSourceFile(name).GetStream();

            // Act
            var result = new SuccessResult(parser, stream, name, Encoding.UTF8).Result;

            // Assert
            Assert.Equal(namespaceName, result.Namespace);
        }

        [InlineData("MissingBrackets1.txt", "closing bracket", 9, 1)]
        [Theory]
        public void ParserHasPredictedError(string name, string errorSubstring, long line, long column)
        {
            // Arrange
            using var stream = new EmbeddedSourceFile(name).GetStream();

            // Act
            var error = (ParserResult<CompilationUnit, ParserState>.Failure)runParserOnStream(parser, ParserState.Default, name, stream, Encoding.UTF8);

            // Assert
            Assert.Contains(errorSubstring, error.Item1);
            Assert.Equal(line, error.Item2.Position.Line);
            Assert.Equal(column, error.Item2.Position.Column);
        }

        /*
        [InlineData()]
        [Theory]
        public void ParserForIndividualFilesHasUnresolvedSymbols(string name, string[] unresolvedSymbols)
        {
        }
        */
    }
}
