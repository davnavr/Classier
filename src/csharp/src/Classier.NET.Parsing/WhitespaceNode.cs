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
    public class WhitespaceNode : ISyntaxNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="WhitespaceNode"/> class.
        /// </summary>
        /// <param name="tokens">The tokens containing the whitespace.</param>
        /// <exception cref="ArgumentNullException"><paramref name="tokens"/> is <see langword="null"/>.</exception>
        public WhitespaceNode(IEnumerable<Token> tokens)
        {
            this.Tokens = new TokenList(tokens?.TakeWhile(tok => tok.TokenType == TokenType.Whitespace));
        }

        /// <inheritdoc/>
        public IReadOnlyList<Token> Tokens { get; }

        /// <summary>
        /// Gets an empty list of nodes.
        /// </summary>
        IReadOnlyList<ISyntaxNode> ISyntaxNode.Nodes { get; } = new List<ISyntaxNode>();

        /// <summary>
        /// Returns the content of this <see cref="WhitespaceNode"/> node.
        /// </summary>
        /// <returns>A <see cref="string"/> containing the whitespace.</returns>
        public sealed override string ToString() => this.Tokens.ToString();
    }
}
