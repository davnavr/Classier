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
    using static FParsec.CharParsers;

    /// <summary>
    /// Wraps a <see cref="ParserResult{Result, UserState}"/>, and assumes it is a <see cref="ParserResult{Result, UserState}.Success"/>.
    /// </summary>
    /// <typeparam name="TResult">The result.</typeparam>
    /// <typeparam name="TUserState">The state.</typeparam>
    public sealed class SuccessResult<TResult, TUserState>
    {
        private readonly ParserResult<TResult, TUserState> result;

        public SuccessResult(Func<ParserResult<TResult, TUserState>> resultEvaluator)
        {
            this.result = resultEvaluator();
        }

        public TResult Result => this.CastResult().Item1;

        private ParserResult<TResult, TUserState>.Success CastResult()
        {
            return this.result switch
            {
                ParserResult<TResult, TUserState>.Success success => success,
                ParserResult<TResult, TUserState>.Failure failure => throw new InvalidCastException(failure.Item2.ToString()),
                _ => throw new InvalidOperationException($"Unknown result type {this.result.GetType()}."),
            };
        }
    }
}
