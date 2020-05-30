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
    using FParsec;
    using Microsoft.FSharp.Core;
    using static Classier.NET.Compiler.Parsing.Grammar;
    using static FParsec.CharParsers;

    /// <summary>
    /// Wraps a parser result and assumes it is a success.
    /// </summary>
    public sealed class SuccessResult
    {
        private readonly ParserResult<CompilationUnit, ParserState> result;

        public SuccessResult(Func<ParserResult<CompilationUnit, ParserState>> resultEvaluator)
        {
            this.result = resultEvaluator();
        }

        public SuccessResult(FSharpFunc<CharStream<ParserState>, Reply<CompilationUnit>> parser, Stream stream, string streamName, Encoding encoding)
            : this(() => runParserOnStream(parser, ParserStateModule.defaultState, streamName, stream, encoding))
        {
        }

        public CompilationUnit Result => this.CastResult().Item1;

        public ParserState State => this.CastResult().Item2;

        private ParserResult<CompilationUnit, ParserState>.Success CastResult()
        {
            return this.result switch
            {
                ParserResult<CompilationUnit, ParserState>.Success success => success,
                ParserResult<CompilationUnit, ParserState>.Failure failure => throw new InvalidCastException(failure.Item2.ToString()),
                _ => throw new InvalidOperationException($"Unknown result type {this.result.GetType()}."),
            };
        }
    }
}
