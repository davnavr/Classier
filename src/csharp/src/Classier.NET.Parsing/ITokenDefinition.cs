﻿/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing
{
    /// <summary>
    /// Determines whether tokens can be created.
    /// </summary>
    /// <typeparam name="T">The type of the value indicating the type of the token.</typeparam>
    public interface ITokenDefinition<T>
    {
        /// <summary>
        /// Gets the length of a token if it was to be created, beginning from the first character in the line.
        /// </summary>
        /// <param name="content">The <see cref="string"/> containing the token.</param>
        /// <returns>The length of the token beginning from the first character of the <see cref="string"/>; or <c>0</c> if a token cannot be created.</returns>
        int GetTokenLength(string content);

        /// <summary>
        /// Gets the type of the token.
        /// </summary>
        /// <param name="token">The token.</param>
        /// <returns>The type of the token.</returns>
        T GetTokenType(Token<T> token);
    }
}
