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
    using SuccessResult = SuccessResult<SyntaxNode.SyntaxNode<Grammar.NodeValue>, Microsoft.FSharp.Core.Unit>;

    public class ParserTests
    {
        [InlineData("Classier.NET.Compiler.source.MyClass1.txt")]
        [InlineData("Classier.NET.Compiler.source.MyModule1.txt")]
        [Theory]
        public void ParserCorrectlyParsesTestFiles(string file) ////, string[] imports)
        {
            // Arrange
            using var stream = typeof(ParserTests).Assembly.GetManifestResourceStream(file);
            using var reader = new StreamReader(stream);
            string content = reader.ReadToEnd();

            // Act
            var result = new SuccessResult(() => runParserOnString(Grammar.parser, null, file, content)).Result;

            // Assert
            var cunode = Assert.IsType<NodeValue.CompilationUnit>(result.Value);
            ////Assert.Equal(imports, cunode.Item.Imports);
            Assert.Equal(normalizeNewlines(content), result.ToString());
        }
    }
}
