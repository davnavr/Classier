/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing
{
    using System;

    /// <summary>
    /// The token definition used when no other token definition had a match.
    /// This class cannot be inherited.
    /// </summary>
    internal sealed class UnknownTokenDefinition : ITokenDefinition
    {
        /// <inheritdoc/>
        public TokenType Type => default;

        /// <summary>
        /// Returns the length of the content.
        /// </summary>
        /// <param name="content">The <see cref="string"/> containing the unknown token.</param>
        /// <returns>The length of <paramref name="content"/>.</returns>
        /// <exception cref="ArgumentNullException"><paramref name="content"/> is <see langword="null"/>.</exception>
        public int GetTokenLength(string content)
        {
            if (content == null)
            {
                throw new ArgumentNullException(nameof(content));
            }

            return content.Length;
        }
    }
}
