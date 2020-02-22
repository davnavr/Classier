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
    /// Represents whitespace spanning one or more tokens.
    /// </summary>
    public class Whitespace : ISyntaxNode
    {
        private readonly TokenList tokenList;

        /// <summary>
        /// Initializes a new instance of the <see cref="Whitespace"/> class.
        /// </summary>
        /// <param name="tokens">The tokens containing the whitespace.</param>
        /// <exception cref="ArgumentNullException"><paramref name="tokens"/> is <see langword="null"/>.</exception>
        public Whitespace(IEnumerable<Token> tokens)
        {
            this.tokenList = new TokenList(tokens?.TakeWhile(tok => tok.TokenType == TokenType.Whitespace));
        }

        /// <summary>
        /// Gets the number of tokens that make up this whitespace node.
        /// </summary>
        public int Count => this.tokenList.Count;

        /// <inheritdoc/>
        public IEnumerator<Token> GetEnumerator() => this.tokenList.GetEnumerator();

        /// <summary>
        /// Returns the content of this <see cref="Whitespace"/> node.
        /// </summary>
        /// <returns>A <see cref="string"/> containing the whitespace.</returns>
        public sealed override string ToString() => this.tokenList.ToString();

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
