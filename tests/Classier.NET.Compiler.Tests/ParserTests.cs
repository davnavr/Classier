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
    using System.Linq;
    using Microsoft.FSharp.Core;
    using Xunit;
    using static Classier.NET.Compiler.Grammar.Lexical;
    using static Classier.NET.Compiler.Grammar.Syntactic;
    using static Classier.NET.Compiler.Matching;
    using static Classier.NET.Compiler.Parser;
    using static Classier.NET.Compiler.Tokenizer;

    public class ParserTests
    {
        [InlineData(@"
public class MyClass {

}
")]
        [Theory]
        public void NodeContentMatchesOriginalInput(string input)
        {
            // Act
            var node = parse(
                parser,
                tokenize(
                    tokenizer,
                    input));

            // Assert
            Assert.Equal(input, Node.toString(FuncConvert.FromFunc<Token, string>(token => token.Content), node.Item1, node.Item2));
        }
    }
}
