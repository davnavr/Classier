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
    public class MultiLineComment : ISyntaxNode
    {
        private readonly TokenList tokenList;

        /// <summary>
        /// Initializes a new instance of the <see cref="MultiLineComment"/> class.
        /// </summary>
        /// <param name="tokens">The tokens containing the multi-line comment.</param>
        /// <exception cref="ArgumentNullException"><paramref name="tokens"/> is <see langword="null"/>.</exception>
        public MultiLineComment(IEnumerable<Token> tokens)
        {
            if (tokens == null)
            {
                throw new ArgumentNullException(nameof(tokens));
            }

            List<Token> list = new List<Token>();
            list.AddRange(tokens.TakeWhile(tok => tok.TokenType == TokenType.CommentStart));
            list.AddRange(tokens.TakeWhile(tok => tok.TokenType != TokenType.CommentEnd));
            list.AddRange(tokens.TakeWhile(tok => tok.TokenType == TokenType.CommentEnd));
            this.tokenList = new TokenList(list);
        }

        /// <summary>
        /// Gets the token indicating the start of the multi-line comment.
        /// </summary>
        public Token StartToken => this.tokenList[0];

        /// <summary>
        /// Gets the token indicating the end of the multi-line comment.
        /// </summary>
        public Token EndToken => this.tokenList[this.tokenList.Count - 1];

        /// <summary>
        /// Gets the number of tokens that make up this multi-line comment.
        /// </summary>
        public int Count => throw new NotImplementedException();

        /// <inheritdoc/>
        public IEnumerator<Token> GetEnumerator() => this.tokenList.GetEnumerator();

        public sealed override string ToString() => this.tokenList.ToString();

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
