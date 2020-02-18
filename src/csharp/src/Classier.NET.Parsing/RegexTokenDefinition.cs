/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing
{
    using System;
    using System.Text.RegularExpressions;

    /// <summary>
    /// Creates tokens from a regular expression.
    /// </summary>
    public class RegexTokenDefinition : ITokenDefinition
    {
        private readonly Regex expression;

        private readonly ClassierTokenType tokenType;

        /// <summary>
        /// Initializes a new instance of the <see cref="RegexTokenDefinition"/> class from the regular expression pattern.
        /// </summary>
        /// <param name="tokenType">The type of the token.</param>
        /// <param name="pattern">The regular expression pattern used to match the tokens.</param>
        /// <exception cref="ArgumentException"><paramref name="pattern"/> is <see langword="null"/> or empty.</exception>
        public RegexTokenDefinition(ClassierTokenType tokenType, string pattern)
        {
            if (string.IsNullOrEmpty(pattern))
            {
                throw new ArgumentException("The regular expression pattern must not be null or empty.", nameof(pattern));
            }

            this.expression = new Regex(pattern);
            this.tokenType = tokenType;
        }

        public int GetTokenLength(string content)
        {
            Match match = this.expression.Match(content);
            return match.Index == 0 ? match.Length : 0;
        }

        /// <summary>
        /// Gets the type of the token.
        /// </summary>
        /// <param name="token">This parameter is ignored.</param>
        /// <returns>The type of the token.</returns>
        public ClassierTokenType GetTokenType(Token token) => this.tokenType;
    }
}
