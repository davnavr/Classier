﻿/*
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
        [InlineData(new[] { "all", "is", "well" }, "not")]
        [InlineData(new[] { "it", "no", "working" }, "because")]
        [Theory]
        public void MatchAnyOfFailsWithLastResult(string[] matches, string text)
        {
            // Arrange
            var matchStrFunc = new InteropFunc<string, MatchFunc<char>>(str => matchStr(str));

            // Act
            var failure = (MatchResult<char>.Failure)result(matchAnyOf(matches, matchStrFunc), itemFrom(text));

            // Assert
            Assert.Contains(matches[matches.Length - 1], failure.Item1);
            Assert.Equal(0, failure.Item2.Index);
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
            // Act
            var success = (MatchResult<char>.Success)result(matchMany(matchStr(expected)), itemFrom(text));

            // Assert
            Assert.Equal(expected.Length * repeatCount, success.Item.Index);
            Assert.Equal(string.Concat(Enumerable.Repeat(expected, repeatCount)), text.Substring(0, expected.Length * repeatCount));
        }

        [InlineData("sedan", "truck", 0)]
        [InlineData("something", "", 0)]
        [InlineData("success", "success", 7)]
        [InlineData("done", "donut", 0)]
        [InlineData("win", "winner", 3)]
        [Theory]
        public void MatchOptionalIsAlwaysSuccess(string expected, string actual, int expectedIndex)
        {
            // Act
            var success = (MatchResult<char>.Success)result(matchOptional(matchStr(expected)), itemFrom(actual));

            // Assert
            Assert.Equal(expectedIndex, success.Item.Index);
        }

        [InlineData(new[] { "type", "of", "(", "object)" }, "typeof(object)")]
        [Theory]
        public void MatchChainIsSuccess(string[] expected, string actual)
        {
            // Arrange
            var chain = expected.Select(str => matchStr(str));

            // Act
            var success = (MatchResult<char>.Success)result(matchChain(chain), itemFrom(actual));

            // Assert
            Assert.Equal(expected.Select(str => str.Length).Sum(), success.Item.Index);
        }

        [InlineData('T', "Test")]
        [InlineData('\r', "\r\n")]
        [Theory]
        public void MatchCharIsSuccess(char expected, string text)
        {
            // Act
            var success = (MatchResult<char>.Success)result(matchChar(expected), itemFrom(text));

            // Assert
            Assert.Equal(1, success.Item.Index);
        }

        // TODO: Check for the bad char surrounded by quotes ''.
        [InlineData('a', "ABC")]
        [InlineData(' ', "This is testing")]
        [Theory]
        public void MatchCharIsFailureForIncorrectChar(char expected, string text)
        {
            // Act
            var failure = (MatchResult<char>.Failure)result(matchChar(expected), itemFrom(text));
            var message = failure.Item1;

            // Assert
            Assert.Equal(0, failure.Item2.Index);
            Assert.Contains("got", message);
            Assert.Contains($"'{expected}'", message);
        }

        [Fact]
        public void MatchCharIsFailureForEmptyText()
        {
            // Act
            var failure = (MatchResult<char>.Failure)result(matchChar('a'), itemFrom(string.Empty));

            // Assert
            Assert.Equal(0, failure.Item2.Index);
            Assert.Contains("end of", failure.Item1);
        }

        [InlineData("Test", "Test")]
        [InlineData("s", "string")]
        [InlineData("class MyClass", "class MyClass extends")]
        [Theory]
        public void MatchStrIsSuccess(string expected, string text)
        {
            // Act
            var success = (MatchResult<char>.Success)result(matchStr(expected), itemFrom(text));

            // Assert
            Assert.Equal(expected.Length, success.Item.Index);
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
            Assert.Contains($"'{badChar}'", message);
        }
    }
}
