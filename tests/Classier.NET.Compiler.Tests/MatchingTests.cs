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
    using FailureResult = Classier.NET.Compiler.FailureResult<char, string>;
    using SuccessResult = Classier.NET.Compiler.SuccessResult<char, string>;

    public class MatchingTests
    {
        [InlineData(new[] { "all", "is", "well" }, "not")]
        [InlineData(new[] { "it", "no", "working" }, "because")]
        [Theory]
        public void MatchAnyOfFailsWithLastResult(string[] matches, string text)
        {
            // Arrange
            var func = new InteropFunc<string, MatchFunc<char, string>>(matchStr);

            // Act
            var failure = new FailureResult(
                    matchAnyOf(matches, func),
                    itemFrom(text));

            // Assert
            Assert.Contains(matches[matches.Length - 1], failure.Message);
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
            var success = new SuccessResult(
                    matchStrSeq(
                        matchMany(
                            matchStr(expected))),
                    itemFrom(text));

            // Assert
            Assert.Equal(expected.Length * repeatCount, success.Item.Index);
            Assert.Equal(text.Substring(0, expected.Length * repeatCount), success.Result);
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
            var success = new SuccessResult(
                matchStrOptional(
                    matchOptional(
                        matchStr(expected))),
                itemFrom(actual));

            // Assert
            Assert.Equal(expectedIndex, success.Item.Index);
        }

        [InlineData(new[] { "type", "of", "(", "object)" }, "typeof(object)")]
        [InlineData(new[] { "one fish" }, "one fish")]
        [InlineData(new[] { "bad" }, "bad syntax")]
        [InlineData(new string[0], "")]
        [InlineData(new string[0], "whatever I want to be")]
        [Theory]
        public void MatchChainIsSuccessIfAllMatchesSucceed(string[] expected, string actual)
        {
            // Act
            var success = new SuccessResult(
                matchStrSeq(
                    matchChain(
                        expected
                        .Select(str => matchStr(str)))),
                itemFrom(actual));

            // Assert
            Assert.Equal(expected.Select(str => str.Length).Sum(), success.Item.Index);
        }

        [InlineData(new[] { "good", "design" }, "good design", ' ')]
        [InlineData(new[] { "wonderful UI" }, "bad UI", 'w')]
        [Theory]
        public void MatchChainIsFailureIfAnyMatchFails(string[] expected, string actual, char badChar)
        {
            // Act
            var failure = new FailureResult(
                matchStrSeq(
                    matchChain(
                        expected
                        .Select(str => matchStr(str)))),
                itemFrom(actual));

            // Assert
            Assert.Contains($"'{badChar}'", failure.Message);
            Assert.StartsWith("chain [", failure.Label);
        }

        [InlineData("indented", "\t\t\tindented", 3)]
        [InlineData("one", "onetoomany", 0)]
        [InlineData("+", "1 + 1", 2)]
        [Theory]
        public void MatchUntilIsSuccessAndExcludesFinalMatch(string untilStr, string actual, int expectedIndex)
        {
            // Act
            var success = new SuccessResult<char, IEnumerable<char>>(
                matchUntil(
                    matchStr(untilStr)),
                itemFrom(actual));

            // Assert
            Assert.Equal(expectedIndex, success.Item.Index);
            Assert.Equal(actual.Substring(success.Result.Count(), untilStr.Length), untilStr);
        }

        [InlineData("content", "conten")]
        [InlineData("123", "EFGHI")]
        [Theory]
        public void MatchUntilIsFailureForNoMatch(string expected, string actual)
        {
            // Act
            var failure = new FailureResult(
                matchCharSeq(
                    matchUntil(
                        matchStr(expected))),
                itemFrom(actual));

            // Assert
            Assert.Contains("until ", failure.Label);
        }

        [InlineData("Data", "[InlineData]", 11)]
        [InlineData("nbsp", "nbsp", 4)]
        [InlineData("matchStr", "\n\nmatchStr(expected)", 10)]
        [InlineData("z", "abcxyz", 6)]
        [InlineData("es", "languages", 9)]
        [InlineData("1", "012345678910", 2)]
        [Theory]
        public void MatchToIsSuccessAndIncludesFinalMatch(string expected, string actual, int expectedIndex)
        {
            // Act
            var success = new SuccessResult<char, Tuple<IEnumerable<char>, string>>(
                matchTo(
                    matchStr(expected)),
                itemFrom(actual));
            var (skippedChars, result) = success.Result;

            // Assert
            Assert.Equal(expectedIndex, success.Item.Index);
            Assert.Equal(actual.Substring(skippedChars.Count(), expected.Length), result);
        }

        [InlineData("System", "Xunit.Abstractions", 's')]
        [InlineData("digital", "analog", 'g')]
        [Theory]
        public void MatchToIsFailureWhenNoSuccessFound(string expected, string actual, char badChar)
        {
            // Act
            var failure = new FailureResult<char, Tuple<IEnumerable<char>, string>>(
                matchTo(
                    matchStr(expected)),
                itemFrom(actual));

            // Assert
            Assert.StartsWith("to ", failure.Label);
            Assert.Contains($"'{badChar}'", failure.Message);
        }

        [Fact]
        public void MatchToIsFailureForEmptyString()
        {
            // Act
            var failure = new FailureResult<char, Tuple<IEnumerable<char>, string>>(
                matchTo(
                    matchStr("not emptyiness")),
                itemFrom(string.Empty));

            // Assert
            Assert.StartsWith("to ", failure.Label);
            Assert.Contains("end of", failure.Message);
        }

        [InlineData("crafting", "circles", "mining & crafting", 17)]
        [InlineData("no", " ", "hasnospaces", 5)]
        [Theory]
        public void MatchWithoutIsSuccessWhenFilterFails(string expected, string without, string actual, int expectedIndex)
        {
            // Act
            var success = new SuccessResult(
                matchWithout(
                    matchStr(without),
                    matchCharSeqAndStr(
                        matchTo(
                            matchStr(expected)))),
                itemFrom(actual));

            // Assert
            Assert.Equal(expectedIndex, success.Item.Index);
        }

        [InlineData("has_tabs", "\t", "should\tnot_has_tabs")]
        [InlineData("</head>", "<script>", "<script>Uh oh</script></head>")]
        [Theory]
        public void MatchWithoutIsFailureWhenFilterIsSuccess(string expected, string without, string actual)
        {
            // Act
            var failure = new FailureResult(matchWithout(matchStr(without), matchCharSeqAndStr(matchTo(matchStr(expected)))), itemFrom(actual));

            // Assert
            Assert.Contains(" without ", failure.Label);
        }

        [InlineData('T', "Test")]
        [InlineData('\r', "\r\n")]
        [Theory]
        public void MatchCharIsSuccessForMatching(char expected, string text)
        {
            // Act
            var success = new SuccessResult(
                    matchChar(expected),
                    itemFrom(text));

            // Assert
            Assert.Equal(1, success.Item.Index);
            Assert.Equal(text[0], success.Result[0]);
        }

        [InlineData('a', "ABC")]
        [InlineData(' ', "This is testing")]
        [Theory]
        public void MatchCharIsFailureForIncorrectChar(char expected, string text)
        {
            // Act
            var failure = new FailureResult(
                    matchChar(expected),
                    itemFrom(text));

            // Assert
            Assert.Contains($"Unexpected '{text[0]}'", failure.Message);
            Assert.Contains($"char '{expected}'", failure.Label);
        }

        [Fact]
        public void MatchCharIsFailureForEmptyText()
        {
            // Act
            var failure = new FailureResult(
                    matchChar('a'),
                    itemFrom(string.Empty));

            // Assert
            Assert.Contains("end of", failure.Message);
        }

        [InlineData("Test", "Test")]
        [InlineData("s", "string")]
        [InlineData("class MyClass", "class MyClass extends")]
        [Theory]
        public void MatchStrIsSuccessForMatching(string expected, string text)
        {
            // Act
            var success = new SuccessResult(
                    matchStr(expected),
                    itemFrom(text));

            // Assert
            Assert.Equal(expected.Length, success.Item.Index);
            Assert.Equal(expected, success.Result);
        }

        [InlineData("")]
        [InlineData("hello")]
        [Theory]
        public void MatchStrIsSuccessWhenMatchingEmptyString(string text)
        {
            // Arrange
            Item<char> startItem = itemFrom(text);

            // Act
            var success = new SuccessResult(
                    matchStr(string.Empty),
                    startItem);

            // Assert
            Assert.Equal(startItem.Index, success.Item.Index);
            Assert.Empty(success.Result);
        }

        [InlineData("error", "erro")]
        [InlineData("oops", "")]
        [Theory]
        public void MatchStrIsFailureForEndOfText(string expected, string text)
        {
            // Arrange
            Item<char> startItem = itemFrom(text);

            // Act
            var failure = new FailureResult(
                    matchStr(expected),
                    startItem);

            // Assert
            Assert.Contains("end of the sequence", failure.Message);
            Assert.Equal($"string '{expected}'", failure.Label);
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
            var failure = new FailureResult(
                    matchStr(expected),
                    startItem);

            // Assert
            Assert.Contains($"Unexpected '{badChar}'", failure.Message);
            Assert.Equal($"string '{expected}'", failure.Label);
        }
    }
}
