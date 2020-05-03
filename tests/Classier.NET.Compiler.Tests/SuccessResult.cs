/*
 * Copyright (c) 2020 NAME HERE
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
    using static FParsec.CharParsers;
    using static FParsec.Primitives;

    /// <summary>
    /// Wraps a <see cref="ParserResult{Result, UserState}"/>, and assumes it is a <see cref="ParserResult{Result, UserState}.Success"/>.
    /// </summary>
    /// <typeparam name="TResult">The result.</typeparam>
    /// <typeparam name="TUserState">The state.</typeparam>
    public sealed class SuccessResult<TResult, TUserState>
    {
        private ParserResult<TResult, TUserState> result;

        public SuccessResult(Func<ParserResult<TResult, TUserState>> resultEvaluator)
        {
            this.result = resultEvaluator();
        }

        public TResult Result => this.CastResult().Item1;

        private ParserResult<TResult, TUserState>.Success CastResult()
        {
            switch (this.result)
            {
                case ParserResult<TResult, TUserState>.Success success:
                    return success;
                case ParserResult<TResult, TUserState>.Failure failiure:
                    throw new InvalidCastException(failiure.Item2.ToString());
                default:
                    throw new InvalidOperationException($"Unknown result type {this.result.GetType()}.");
            }
        }
    }
}
