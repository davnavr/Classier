/*
 * Copyright (c) 2020, David Navarro.
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
            this.func = func ?? throw new ArgumentNullException(nameof(func));
        }

        public override TResult Invoke(T func) => this.func.Invoke(func);
    }
}
