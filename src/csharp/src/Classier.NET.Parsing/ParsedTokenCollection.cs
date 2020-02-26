/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Linq;

    /// <summary>
    /// Represents a collection of syntax nodes parsed from tokens.
    /// This class cannot be inherited.
    /// </summary>
    internal sealed class ParsedTokenCollection : IEnumerable<ISyntaxNode>
    {
        private readonly IEnumerable<Token> tokens;

        /// <summary>
        /// Initializes a new instance of the <see cref="ParsedTokenCollection"/> class.
        /// </summary>
        /// <param name="tokens">The tokens to parse.</param>
        /// <exception cref="ArgumentNullException"><paramref name="tokens"/> is <see langword="null"/>.</exception>
        internal ParsedTokenCollection(IEnumerable<Token> tokens)
        {
            this.tokens = tokens ?? throw new ArgumentNullException(nameof(tokens));
        }

        /// <inheritdoc/>
        public IEnumerator<ISyntaxNode> GetEnumerator()
        {
            throw new NotImplementedException();
        }

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
