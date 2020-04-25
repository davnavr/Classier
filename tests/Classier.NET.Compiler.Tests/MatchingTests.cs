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
    using static Classier.NET.Compiler.Matching;
    using static Classier.NET.Compiler.Tokenizer;

    public class MatchingTests
    {
        [InlineData(new[] { "all", "is", "well" }, "not")]
        [InlineData(new[] { "it", "no", "working" }, "because")]
        [Theory]
        public void MatchAnyOfFailsWithLastResult(string[] matches, string text)
        {
            // Act
            var failure = new FailureResult<char>(
                    matchAnyOf(
                        matches,
                        FuncConvert.FromFunc<string, MatchFunc<char>>(matchStr)),
                    Item.ofSeq(text));

            // Assert
            Assert.Contains(matches[matches.Length - 1], failure.Label);
        }

        [InlineData("return", "returnreturnret", 2)]
        [InlineData("lambda", "lambdalambdalambdal", 3)]
        [Theory]
        public void MatchManyIsSuccessForPartOfString(string expected, string text, int repeatCount)
        {
            // Act
            var success = new SuccessResult<char>(
                matchMany(
                    matchStr(expected)),
                Item.ofSeq(text));

            // Assert
            Assert.Equal(expected.Length * repeatCount, success.Item.Index);
            Assert.Equal(text.Substring(0, expected.Length * repeatCount), success.Result);
        }

        [InlineData("cs", "cscscs")]
        [InlineData("work", "work")]
        [InlineData("two", "twotwo")]
        [InlineData(" ", "      ")]
        [InlineData("menu", "menumenumenumenumenumenumenumenumenumenumenumenu")]
        [Theory]
        public void MatchManyIsSuccessForEntireStringFor(string expected, string text)
        {
            // Act
            var success = new SuccessResult<char>(
                matchMany(
                    matchStr(expected)),
                Item.ofSeq(text));

            // Assert
            Assert.False(success.HasItem);
            Assert.Equal(text, success.Result);
        }

        [InlineData("sedan", "truck", 0)]
        [InlineData("done", "donut", 0)]
        [InlineData("win", "winner", 3)]
        [InlineData("\u5678", "\u9101", 0)]
        [Theory]
        public void MatchOptionalIsSuccessForPartOfString(string expected, string actual, int expectedIndex)
        {
            // Act
            var success = new SuccessResult<char>(
                matchOptional(
                    matchStr(expected)),
                Item.ofSeq(actual));

            // Assert
            Assert.Equal(expectedIndex, success.Item.Index);
        }

        [InlineData("success", "success")]
        [InlineData("\u1234", "\u1234")]
        [Theory]
        public void MatchOptionalIsSuccessForEntireString(string expected, string actual)
        {
            // Act
            var success = new SuccessResult<char>(
                matchOptional(
                    matchStr(expected)),
                Item.ofSeq(actual));

            // Assert
            Assert.False(success.HasItem);
            Assert.Equal(actual, success.Result);
        }

        [InlineData(new[] { "bad" }, "bad syntax", 3)]
        [InlineData(new[] { "what", "ever", " ", "I" }, "whatever I want to be", 10)]
        [InlineData(new string[0], "whatever I want to be", 0)]
        [Theory]
        public void MatchChainIsSuccessForPartOfStringIfAllMatchesSucceed(string[] expected, string actual, int expectedIndex)
        {
            // Act
            var success = new SuccessResult<char>(
                matchChain(
                    expected
                    .Select(str => matchStr(str))),
                Item.ofSeq(actual));

            // Assert
            Assert.Equal(expectedIndex, success.Item.Index);
            Assert.Equal(actual.Substring(0, expectedIndex), success.Result);
        }

        [InlineData(new[] { "type", "of", "(", "object)" }, "typeof(object)")]
        [InlineData(new[] { "one fish" }, "one fish")]
        [Theory]
        public void MatchChainIsSuccessForEntireStringIfAllMatchesSucceed(string[] expected, string actual)
        {
            // Act
            var success = new SuccessResult<char>(
                matchChain(
                    expected
                    .Select(str => matchStr(str))),
                Item.ofSeq(actual));

            // Assert
            Assert.False(success.HasItem);
            Assert.Equal(actual, success.Result);
        }

        [InlineData(new[] { "good", "design" }, "good design", ' ')]
        [InlineData(new[] { "wonderful UI" }, "bad UI", 'b')]
        [Theory]
        public void MatchChainIsFailureIfAnyMatchFails(string[] expected, string actual, char badChar)
        {
            // Act
            var failure = new FailureResult<char>(
                matchChain(
                    expected
                    .Select(str => matchStr(str))),
                Item.ofSeq(actual));

            // Assert
            Assert.Contains($"'{badChar}'", failure.Message);
            Assert.StartsWith("chain [", failure.Label);
        }

        [InlineData("one", "onetoomany", 0)]
        [InlineData("+", "1 + 1", 2)]
        [InlineData("the", "be the roundabout", 3)]
        [Theory]
        public void MatchUntilIsSuccessForPartOfStringAndExcludesFinalMatch(string untilStr, string actual, int expectedIndex)
        {
            // Act
            var success = new SuccessResult<char>(
                matchUntil(
                    matchStr(untilStr)),
                Item.ofSeq(actual));

            // Assert
            Assert.Equal(expectedIndex, success.Item.Index);
            Assert.Equal(actual.Substring(success.Result.Count(), untilStr.Length), untilStr);
        }

        [InlineData("indented", "\t\t\tindented", 3)]
        [InlineData("end", "if true then print('wow') end", 26)]
        [Theory]
        public void MatchUntilIsSuccessForEntireStringAndExcludesFinalMatch(string untilStr, string actual, int expectedIndex)
        {
            // Act
            var success = new SuccessResult<char>(
                matchUntil(
                    matchStr(untilStr)),
                Item.ofSeq(actual));

            // Assert
            Assert.Equal(expectedIndex, success.Item.Index);
            Assert.EndsWith(untilStr, actual);
        }

        [InlineData("content", "conten")]
        [InlineData("123", "EFGHI")]
        [Theory]
        public void MatchUntilIsFailureForNoMatch(string expected, string actual)
        {
            // Act
            var failure = new FailureResult<char>(
                matchUntil(
                    matchStr(expected)),
                Item.ofSeq(actual));

            // Assert
            Assert.Contains("until ", failure.Label);
        }

        [InlineData("Data", "[InlineData]", 11)]
        [InlineData("matchStr", "\n\nmatchStr(expected)", 10)]
        [InlineData("1", "012345678910", 2)]
        [InlineData("3", "123-123-123", 3)]
        [Theory]
        public void MatchToIsSuccessForPartOfStringAndIncludesFinalMatch(string expected, string actual, int expectedIndex)
        {
            // Act
            var success = new SuccessResult<char>(
                matchTo(
                    matchStr(expected)),
                Item.ofSeq(actual));

            // Assert
            Assert.Equal(expectedIndex, success.Item.Index);
            Assert.Equal(actual.Substring(0, expectedIndex), success.Result);
        }

        [InlineData("nbsp", "nbsp")]
        [InlineData("z", "abcxyz")]
        [InlineData("es", "languages")]
        [Theory]
        public void MatchToIsSuccessForEntireStringAndIncludesFinalMatch(string expected, string actual)
        {
            // Act
            var success = new SuccessResult<char>(
                matchTo(
                    matchStr(expected)),
                Item.ofSeq(actual));

            // Assert
            Assert.False(success.HasItem);
            Assert.Equal(actual, success.Result);
        }

        [InlineData("System", "Xunit.Abstractions", 's')]
        [InlineData("digital", "analog", 'g')]
        [Theory]
        public void MatchToIsFailureWhenNoSuccessFound(string expected, string actual, char badChar)
        {
            // Act
            var failure = new FailureResult<char>(
                matchTo(
                    matchStr(expected)),
                Item.ofSeq(actual));

            // Assert
            Assert.StartsWith("to ", failure.Label);
            Assert.Contains($"'{badChar}'", failure.Message);
        }

        [Fact]
        public void MatchToIsFailureForEmptyString()
        {
            // Act
            var failure = new FailureResult<char>(
                matchTo(
                    matchStr("not emptyiness")),
                Item.ofSeq(string.Empty));

            // Assert
            Assert.StartsWith("to ", failure.Label);
            Assert.Contains("end of", failure.Message);
        }

        [InlineData("To the end of the road.")]
        [InlineData("am I success?")]
        [Theory]
        public void MatchToEndIsAlwaysSuccessful(string text)
        {
            // Act
            var success = new SuccessResult<char>(
                matchToEnd<char>(),
                Item.ofSeq(text));

            // Assert
            Assert.False(success.HasItem);
            Assert.Equal(text, success.Result);
        }

        [InlineData("circles", "mining & crafting")]
        [InlineData(" ", "hasnospaces")]
        [Theory]
        public void MatchWithoutIsSuccessWhenFilterFails(string without, string actual)
        {
            // Act
            var success = new SuccessResult<char>(
                matchWithout(
                    matchStr(without),
                    matchStr(actual)),
                Item.ofSeq(actual));

            // Assert
            Assert.False(success.HasItem);
            Assert.Equal(actual, success.Result);
        }

        [InlineData("has_tabs", "\t", "should\tnot_has_tabs")]
        [InlineData("</head>", "<script>", "<script>Uh oh</script></head>")]
        [Theory]
        public void MatchWithoutIsFailureWhenFilterIsSuccess(string expected, string without, string actual)
        {
            // Act
            var failure = new FailureResult<char>(
                matchWithout(
                    matchStr(without),
                    matchTo(
                        matchStr(expected))),
                Item.ofSeq(actual));

            // Assert
            Assert.Contains(" without ", failure.Label);
        }

        // TODO: Move tests for char and str in LexingTests.
        [InlineData('T', "Test")]
        [InlineData('\r', "\r\n")]
        [Theory]
        public void MatchCharIsSuccessForMatching(char expected, string text)
        {
            // Act
            var success = new SuccessResult<char>(
                    matchChar(expected),
                    Item.ofSeq(text));

            // Assert
            Assert.Equal(1, success.Item.Index);
            Assert.Equal(text[0], success.Result.First());
        }

        [InlineData('a', "ABC")]
        [InlineData(' ', "This is testing")]
        [Theory]
        public void MatchCharIsFailureForIncorrectChar(char expected, string text)
        {
            // Act
            var failure = new FailureResult<char>(
                    matchChar(expected),
                    Item.ofSeq(text));

            // Assert
            Assert.Contains($"Unexpected '{text[0]}'", failure.Message);
            Assert.Contains($"char '{expected}'", failure.Label);
        }

        [Fact]
        public void MatchCharIsFailureForEmptyText()
        {
            // Act
            var failure = new FailureResult<char>(
                    matchChar('a'),
                    Item.ofSeq(string.Empty));

            // Assert
            Assert.Contains("end of", failure.Message);
        }

        [InlineData("Test", "Test")]
        [InlineData("\r", "\r")]
        [Theory]
        public void MatchStrIsSuccessWhenMatchingEntireString(string expected, string text)
        {
            // Act
            var success = new SuccessResult<char>(
                    matchStr(expected),
                    Item.ofSeq(text));

            // Assert
            Assert.False(success.HasItem);
            Assert.Equal(expected, success.Result);
        }

        [InlineData("s", "string")]
        [InlineData("class MyClass", "class MyClass extends")]
        [Theory]
        public void MatchStrIsSuccessWhenMatchingPartOfString(string expected, string text)
        {
            // Act
            var success = new SuccessResult<char>(
                    matchStr(expected),
                    Item.ofSeq(text));

            // Assert
            Assert.Equal(expected.Length, success.Item.Index);
            Assert.Equal(expected, success.Result);
        }

        [InlineData("\u0085")]
        [InlineData("hello")]
        [Theory]
        public void MatchStrIsSuccessWhenMatchingWithEmptyString(string text)
        {
            // Act
            var success = new SuccessResult<char>(
                    matchStr(string.Empty),
                    Item.ofSeq(text));

            // Assert
            Assert.Equal(0, success.HasItem ? success.Item.Index : 0);
            Assert.Empty(success.Result);
        }

        [InlineData("error", "erro")]
        [InlineData("oops", "")]
        [Theory]
        public void MatchStrIsFailureForEndOfText(string expected, string text)
        {
            // Act
            var failure = new FailureResult<char>(
                    matchStr(expected),
                    Item.ofSeq(text));

            // Assert
            Assert.Contains("end of the sequence", failure.Message);
            Assert.Equal($"string '{expected}'", failure.Label);
        }

        [InlineData("oh no", "oh!no", '!')]
        [InlineData("self", " elf", ' ')]
        [InlineData("abcd", "efgh", 'e')]
        [InlineData("world", "_world", '_')]
        [Theory]
        public void MatchStrIsFailureForIncorrect(string expected, string text, char badChar)
        {
            // Act
            var failure = new FailureResult<char>(
                    matchStr(expected),
                    Item.ofSeq(text));

            // Assert
            Assert.Contains($"Unexpected '{badChar}'", failure.Message);
            Assert.Equal($"string '{expected}'", failure.Label);
        }
    }
}
