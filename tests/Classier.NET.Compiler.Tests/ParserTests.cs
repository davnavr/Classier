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
    using System;
    using System.IO;
    using System.Text;
    using Xunit;
    using static Classier.NET.Compiler.Grammar;
    using static FParsec.CharParsers;
    using SuccessResult = SuccessResult<SyntaxNode.SyntaxNode<Grammar.NodeValue>, Grammar.Flags>;

    public class ParserTests
    {
        [InlineData("Classier.NET.Compiler.source.MyAbstractClass1.txt", new[] { "java", "lang" }, new[] { "java", "util" })]
        [InlineData("Classier.NET.Compiler.source.MyClass1.txt", new[] { "System" }, new[] { "System", "IO" })]
        [InlineData("Classier.NET.Compiler.source.MyModule1.txt", new[] { "system", "reflection" })]
        [Theory]
        public void ParserCorrectlyParsesTestFiles(string file, params string[][] imports)
        {
            // Arrange
            using var stream = typeof(ParserTests).Assembly.GetManifestResourceStream(file);
            using var reader = new StreamReader(stream);
            string content = reader.ReadToEnd();

            // Act
            var result = new SuccessResult(() => runParserOnString(Grammar.parser, Flags.None, file, content)).Result;

            // Assert
            var cunode = Assert.IsType<NodeValue.CompilationUnit>(result.Value);
            Assert.Equal(imports, cunode.Item.Imports);
            //// Assert.Equal(ns, cunode.Item.Namespace);
            Assert.Equal(normalizeNewlines(content), result.ToString());
        }
    }
}
