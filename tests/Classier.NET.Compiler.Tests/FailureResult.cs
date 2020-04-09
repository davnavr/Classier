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
    using static Classier.NET.Compiler.Matching;

    /// <summary>
    /// Wraps a <see cref="MatchResult{Match, Result}"/> value, and assumes it is a <see cref="MatchResult{Match, Result}.Success"/>.
    /// </summary>
    /// <typeparam name="TMatch">The type of the items in the sequence.</typeparam>
    /// <typeparam name="TResult">The type of the item produced from a successful match.</typeparam>
    public sealed class FailureResult<TMatch, TResult>
    {
        private readonly MatchFunc<TMatch, TResult> func;

        private readonly MatchResult<TMatch, TResult> result;

        public FailureResult(MatchFunc<TMatch, TResult> func, Item<TMatch> item)
        {
            this.func = func;
            this.result = evaluateMatch(this.func, item);
        }

        public string Label => this.CastFailure().Item1;

        public string Message => this.CastFailure().Item2;

        private MatchResult<TMatch, TResult>.Failure CastFailure()
        {
            if (this.result is MatchResult<TMatch, TResult>.Success success)
            {
                throw new InvalidOperationException($"Unexpected success when matching ({this.func.Label}), the result is '{success.Item1}'.");
            }

            return (MatchResult<TMatch, TResult>.Failure)this.result;
        }
    }
}
