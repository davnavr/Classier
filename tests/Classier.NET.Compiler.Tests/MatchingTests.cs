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
    using System.Linq;
    using Microsoft.FSharp.Core;
    using Xunit;
    using static Classier.NET.Compiler.Lexing;
    using static Classier.NET.Compiler.Matching;
    using static Classier.NET.Compiler.Program;

    public class MatchingTests
    {
        [InlineData("not", "all", "is", "well")]
        [InlineData("because", "it", "no", "working")]
        [Theory]
        public void MatchAnyOfFailsWithLastResult(string text, params string[] matches)
        {
            // Arrange
            Item<char> startItem = itemFrom(text);
            var matchStrFunc = new InteropFunc<string, MatchFunc<char>>(str => matchStr(str));

            // Act
            var failure = (MatchResult<char>.Failure)result(matchAnyOf(matches, matchStrFunc), startItem);

            // Assert
            Assert.Contains(matches[matches.Length - 1], failure.Item1);
            Assert.Equal(startItem.Index, failure.Item2.Index);
        }

        [InlineData('T', "Test")]
        [InlineData('\r', "\r\n")]
        [Theory]
        public void MatchCharIsSuccess(char expected, string text)
        {
            // Arrange
            Item<char> startItem = itemFrom(text);

            // Act
            var success = (MatchResult<char>.Success)result(matchChar(expected), startItem);

            // Assert
            Assert.Equal(startItem.Index + 1, success.Item.Index);
        }

        [InlineData('a', "ABC")]
        [InlineData(' ', "This is testing")]
        [Theory]
        public void MatchCharIsFailureForIncorrectChar(char expected, string text)
        {
            // Arrange
            Item<char> startItem = itemFrom(text);

            // Act
            var failure = (MatchResult<char>.Failure)result(matchChar(expected), startItem);

            // Assert
            Assert.Equal(startItem.Index, failure.Item2.Index);
            Assert.Contains("got", failure.Item1);
        }

        [Fact]
        public void MatchCharIsFailureForEmptyText()
        {
            // Arrange
            Item<char> startItem = itemFrom(string.Empty);

            // Act
            var failure = (MatchResult<char>.Failure)result(matchChar('a'), startItem);

            // Assert
            Assert.Equal(startItem.Index, failure.Item2.Index);
            Assert.Contains("end of", failure.Item1);
        }

        [InlineData("Test", "Test")]
        [InlineData("s", "string")]
        [InlineData("class MyClass", "class MyClass extends")]
        [Theory]
        public void MatchStrIsSuccess(string expected, string text)
        {
            // Arrange
            Item<char> startItem = itemFrom(text);

            // Act
            var success = (MatchResult<char>.Success)result(matchStr(expected), startItem);

            // Assert
            Assert.Equal(startItem.Index + expected.Length, success.Item.Index);
        }

        [InlineData("")]
        [InlineData("hello")]
        [Theory]
        public void MatchStrIsSuccessForEmptyString(string text)
        {
            // Arrange
            Item<char> startItem = itemFrom(text);

            // Act
            var success = (MatchResult<char>.Success)result(matchStr(string.Empty), startItem);

            // Assert
            Assert.Equal(startItem.Index, success.Item.Index);
        }

        [InlineData("error", "erro")]
        [InlineData("oops", "")]
        [Theory]
        public void MatchStrIsFailureForEndOfText(string expected, string text)
        {
            // Arrange
            Item<char> startItem = itemFrom(text);

            // Act
            var failure = (MatchResult<char>.Failure)result(matchStr(expected), startItem);
            var message = failure.Item1;

            // Assert
            Assert.Equal(startItem.Index, failure.Item2.Index);
            Assert.Contains(expected, message);
            Assert.Contains("end of", message);
        }

        [InlineData("oh no", "oh\rno", '\r')]
        [InlineData("self", " elf", ' ')]
        [InlineData("abcd", "efgh", 'e')]
        [InlineData("world", "_world", '_')]
        [Theory]
        public void MatchStrIsFailureForIncorrect(string expected, string text, char badChar)
        {
            // Arrange
            Item<char> startItem = itemFrom(text);

            // Act
            var failure = (MatchResult<char>.Failure)result(matchStr(expected), startItem);
            var message = failure.Item1;

            // Assert
            Assert.Equal(startItem.Index, failure.Item2.Index);
            Assert.Contains(expected, message);
            Assert.Contains("got", message);
            Assert.Contains(badChar, message.Substring(message.LastIndexOf("got")));
        }

        [InlineData("cs", "cscscs", 3)]
        [InlineData("work", "work", 1)]
        [InlineData("two", "twotwo", 2)]
        [InlineData(" ", "      ", 6)]
        [InlineData("return", "returnreturnret", 2)]
        [InlineData("menu", "menumenumenumenumenumenumenumenumenumenumenumenu", 12)]
        [Theory]
        public void MatchManyIsSuccessForRepeated(string expected, string text, int repeatCount)
        {
            // Arrange
            Item<char> startItem = itemFrom(text);

            // Act
            var success = (MatchResult<char>.Success)result(matchMany(matchStr(expected)), startItem);

            // Assert
            Assert.Equal(startItem.Index + (expected.Length * repeatCount), success.Item.Index);
            Assert.Equal(string.Concat(Enumerable.Repeat(expected, repeatCount)), text.Substring(0, expected.Length * repeatCount));
        }
    }
}
