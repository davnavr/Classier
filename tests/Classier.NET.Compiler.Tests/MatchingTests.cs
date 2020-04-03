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
            var (msg, endItem) = asFailure(result(matchAnyOf(matches, matchStrFunc), startItem));

            // Assert
            Assert.Contains(matches[matches.Length - 1], msg);
            Assert.Equal(itemIndex(startItem), itemIndex(endItem));
        }

        [InlineData('T', "Test")]
        [InlineData('\r', "\r\n")]
        [Theory]
        public void MatchCharIsSuccess(char expected, string text)
        {
            // Arrange
            Item<char> startItem = itemFrom(text);

            // Act
            Item<char> endItem = asSuccess(result(matchChar(expected), startItem));

            // Assert
            Assert.NotEqual(itemIndex(startItem), itemIndex(endItem));
        }

        [InlineData('a', "ABC")]
        [InlineData(' ', "This is testing")]
        [Theory]
        public void MatchCharIsFailureForIncorrectChar(char expected, string text)
        {
            // Arrange
            Item<char> startItem = itemFrom(text);

            // Act
            var (msg, endItem) = asFailure(result(matchChar(expected), startItem));

            // Assert
            Assert.Equal(itemIndex(startItem), itemIndex(endItem));
            Assert.Contains("got", msg);
        }

        [Fact]
        public void MatchCharIsFailureForEmptyText()
        {
            // Arrange
            Item<char> startItem = itemFrom(string.Empty);

            // Act
            var (msg, endItem) = asFailure(result(matchChar('a'), startItem));

            // Assert
            Assert.Equal(itemIndex(startItem), itemIndex(endItem));
            Assert.Contains("end of", msg);
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
            Item<char> endItem = asSuccess(result(matchStr(expected), startItem));

            // Assert
            Assert.NotEqual(itemIndex(startItem), itemIndex(endItem));
        }

        [InlineData("")]
        [InlineData("hello")]
        [Theory]
        public void MatchStrIsSuccessForEmptyString(string text)
        {
            // Arrange
            Item<char> startItem = itemFrom(text);

            // Act
            Item<char> endItem = asSuccess(result(matchStr(string.Empty), startItem));

            // Assert
            Assert.Equal(itemIndex(startItem), itemIndex(endItem));
        }

        [InlineData("error", "erro")]
        [InlineData("oops", "")]
        [Theory]
        public void MatchStrIsFailureForEndOfText(string expected, string text)
        {
            // Arrange
            Item<char> startItem = itemFrom(text);

            // Act
            var (msg, endItem) = asFailure(result(matchStr(expected), startItem));

            // Assert
            Assert.Equal(itemIndex(startItem), itemIndex(endItem));
            Assert.Contains(expected, msg);
            Assert.Contains("end of", msg);
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
            var (msg, endItem) = asFailure(result(matchStr(expected), startItem));

            // Assert
            Assert.Equal(itemIndex(startItem), itemIndex(endItem));
            Assert.Contains(expected, msg);
            Assert.Contains("got", msg);
            Assert.Contains(badChar, msg.Substring(msg.LastIndexOf("got")));
        }
    }
}
