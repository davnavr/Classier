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

        private readonly ITokenDefinition definition;

        /// <summary>
        /// Initializes a new instance of the <see cref="Token"/> struct.
        /// </summary>
        /// <param name="content">The content of the token.</param>
        /// <param name="definition">The <see cref="ITokenDefinition"/> that matched this token.</param>
        /// <exception cref="ArgumentNullException"><paramref name="definition"/> is <see langword="null"/>.</exception>
        /// <exception cref="ArgumentException"><paramref name="content"/> is <see langword="null"/> or empty.</exception>
        public Token(string content, ITokenDefinition definition)
        {
            this.content = !string.IsNullOrEmpty(content) ? content : throw new ArgumentException("The content of the token must not be empty.", nameof(content));
            this.definition = definition ?? throw new ArgumentNullException(nameof(definition));
        }

        /// <summary>
        /// Gets the type of the token.
        /// </summary>
        public ClassierTokenType TokenType => this.definition.GetTokenType(this);

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
