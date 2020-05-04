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
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    using System.Text;
    using Microsoft.FSharp.Core;
    using Xunit;
    using static FParsec.CharParsers;
    using SuccessResult = SuccessResult<Node.Node<Grammar.NodeValue>, Microsoft.FSharp.Core.Unit>;

    public class ParserTests
    {
        [InlineData("Classier.NET.Compiler.source.MyClass1.txt")]
        [InlineData("Classier.NET.Compiler.source.MyModule1.txt")]
        [Theory]
        public void ParserCorrectlyParsesTestFiles(string file)
        {
            // Arrange
            Func<Stream> stream = () => typeof(ParserTests).Assembly.GetManifestResourceStream(file);

            // Act
            var result =
                new SuccessResult(
                    () =>
                    {
                        using Stream fileStream = stream();
                        return runParserOnStream(Grammar.parser, null, file, fileStream, Encoding.UTF8);
                    });

            // Assert
            ////Assert.IsType<NodeValue.CompilationUnit>(result.Result.Value);
            Assert.Equal(result.Result.Content, new StreamChars(stream));
        }
    }
}
