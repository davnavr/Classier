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
    using Microsoft.FSharp.Core;

    /// <summary>
    /// Used to convert a <see cref="Func{TResult, T}"/> to a <see cref="FSharpFunc{T, TResult}"/>.
    /// </summary>
    /// <typeparam name="T">The type of the argument.</typeparam>
    /// <typeparam name="TResult">The type of the return value.</typeparam>
    internal sealed class InteropFunc<T, TResult> : FSharpFunc<T, TResult>
    {
        private readonly Func<T, TResult> func;

        public InteropFunc(Func<T, TResult> func)
        {
            this.func = func;
        }

        public override TResult Invoke(T func) => this.func.Invoke(func);
    }
}
