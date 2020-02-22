/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing
{
    using System;

    /// <summary>
    /// Represents a token.
    /// </summary>
    public readonly struct Token
    {
        private readonly string content;

        /// <summary>
        /// Initializes a new instance of the <see cref="Token"/> struct.
        /// </summary>
        /// <param name="content">The content of the token.</param>
        /// <param name="lineNumber">The line number of the line containing this token.</param>
        /// <param name="type">The type of the token.</param>
        /// <exception cref="ArgumentException"><paramref name="content"/> is <see langword="null"/> or empty.</exception>
        public Token(string content, int lineNumber, TokenType type)
        {
            this.content = !string.IsNullOrEmpty(content) ? content : throw new ArgumentException("The content of the token must not be empty.", nameof(content));
            this.LineNumber = lineNumber;
            this.TokenType = type;
        }

        /// <summary>
        /// Gets the line number of the line containing this token.
        /// </summary>
        public int LineNumber { get; }

        /// <summary>
        /// Gets the type of the token.
        /// </summary>
        public TokenType TokenType { get; }

        /// <summary>
        /// Returns the content of the token.
        /// </summary>
        /// <returns>A <see cref="string"/> containing the content of the token.</returns>
        public override string ToString()
        {
            return this.content;
        }
    }
}
