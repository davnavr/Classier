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
    using Microsoft.FSharp.Core;
    using static Classier.NET.Compiler.Item;
    using static Classier.NET.Compiler.Matching;

    /// <summary>
    /// Wraps a <see cref="MatchResult{T}"/> value, and assumes it is a <see cref="MatchResult{T}.Success"/>.
    /// </summary>
    /// <typeparam name="T">The type of the items in the sequence.</typeparam>
    public sealed class SuccessResult<T>
    {
        private readonly MatchResult<T> result;

        public SuccessResult(MatchFunc<T> func, Item<T> item)
        {
            this.result = evaluateMatch(func, item);
        }

        public Item<T> Item => this.ItemOption.Value;

        public bool HasItem => FSharpOption<Item<T>>.get_IsSome(this.ItemOption);

        public IEnumerable<T> Result => this.CastSuccess().Item1;

        private FSharpOption<Item<T>> ItemOption => this.CastSuccess().Item2;

        private MatchResult<T>.Success CastSuccess()
        {
            if (this.result is MatchResult<T>.Failure failure)
            {
                throw new InvalidOperationException($"Unexpected failure for ({failure.Item1}). {failure.Item2}");
            }

            return (MatchResult<T>.Success)this.result;
        }
    }
}
