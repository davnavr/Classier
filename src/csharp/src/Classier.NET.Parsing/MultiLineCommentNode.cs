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
    /// Represents a multi-line comment.
    /// </summary>
    public class MultiLineCommentNode : ISyntaxNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="MultiLineCommentNode"/> class.
        /// </summary>
        /// <param name="tokens">The tokens containing the multi-line comment.</param>
        /// <exception cref="ArgumentNullException"><paramref name="tokens"/> is <see langword="null"/>.</exception>
        public MultiLineCommentNode(IEnumerable<Token> tokens)
        {
            if (tokens == null)
            {
                throw new ArgumentNullException(nameof(tokens));
            }

            List<Token> list = new List<Token>();
            list.AddRange(tokens.TakeWhile(tok => tok.TokenType == TokenType.CommentStart)); // TODO: Find way to throw exception if conditions aren't met.
            list.AddRange(tokens.TakeWhile(tok => tok.TokenType != TokenType.CommentEnd));
            list.AddRange(tokens.TakeWhile(tok => tok.TokenType == TokenType.CommentEnd));
            this.Tokens = new TokenList(list);
        }

        /// <summary>
        /// Gets the token indicating the start of the multi-line comment.
        /// </summary>
        public Token StartToken => this.Tokens[0];

        /// <summary>
        /// Gets the token indicating the end of the multi-line comment.
        /// </summary>
        public Token EndToken => this.Tokens[this.Tokens.Count - 1];

        /// <inheritdoc/>
        public IReadOnlyList<Token> Tokens { get; }

        /// <summary>
        /// Gets an empty list.
        /// </summary>
        IReadOnlyList<ISyntaxNode> ISyntaxNode.Nodes { get; } = new List<ISyntaxNode>();

        /// <summary>
        /// Gets the content of the multi-line comment.
        /// </summary>
        /// <returns>A <see cref="string"/> containing content of the multi-line comment, along with the symbols indicating the start and the end of the comment.</returns>
        public sealed override string ToString() => this.Tokens.ToString();
    }
}
