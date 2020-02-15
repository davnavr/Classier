/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing
{
    using System;

    /// <summary>
    /// Iterates through tokens.
    /// </summary>
    public interface ITokenIterator : IDisposable
    {
        int CurrentLine { get; }

        int CurrentPosition { get; }

        Token GetNextToken(Action<Token> filter);
    }
}
