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
    using Microsoft.FSharp.Collections;
    using Xunit;
    using static Classier.NET.Compiler.Parsing.Grammar;
    using static FParsec.CharParsers;

    public class ParserTests
    {
        [InlineData("MultipleClasses.txt")]
        [InlineData("MyAbstractClass1.txt", "java.lang", "java.util")]
        [InlineData("MyException1.txt")]
        [InlineData("MyModule1.txt", "system.reflection.Assembly")]
        [InlineData("NoAcessModifiers.txt", "System")]
        [Theory]
        public void ParserCorrectlySetsUsings(string name, params string[] usings)
        {
            // Arrange
            using var stream = new EmbeddedSourceFile(name).GetStream();

            // Act
            var result = new SuccessResult(parser, stream, name, Encoding.UTF8).Result;

            // Assert
            Assert.Equal(usings, result.Usings.Select(names => string.Join('.', names)));
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
        [InlineData("NoCatchOrFinally.txt", "at least one catch", 6, 6)]
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

        [InlineData("MyAbstractClass1.txt", "this.is.my.space")]
        [Theory]
        public void ParserIncludesNamespaceInSymbolTable(string file, string expectedNamespace)
        {
            // Arrange
            using var stream = new EmbeddedSourceFile(file).GetStream();

            // Act
            var namespaces = new SuccessResult(parser, stream, file, Encoding.UTF8).State.Symbols.Namespaces;

            // Assert
            Assert.True(
                namespaces.ContainsKey(
                    ListModule.OfArray(
                        expectedNamespace.Split('.'))));
        }
    }
}
