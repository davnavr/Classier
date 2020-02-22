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
    /// Represents a list of tokens.
    /// This class cannot be inherited.
    /// </summary>
    internal sealed class TokenList : IReadOnlyList<Token>
    {
        private readonly List<Token> list;

        /// <summary>
        /// Initializes a new instance of the <see cref="TokenList"/> class with the specified tokens.
        /// </summary>
        /// <param name="tokens">The tokens in the list.</param>
        /// <exception cref="ArgumentNullException"><paramref name="tokens"/> is <see langword="null"/>.</exception>
        internal TokenList(IEnumerable<Token> tokens)
        {
            this.list = tokens?.ToList() ?? throw new ArgumentNullException(nameof(tokens));
        }

        /// <summary>
        /// Gets the number of tokens in this <see cref="TokenList"/>.
        /// </summary>
        public int Count => this.list.Count;

        /// <summary>
        /// Gets the token at the specified index.
        /// </summary>
        /// <param name="index">The index of the token to retrieve.</param>
        /// <returns>The <see cref="Token"/> at the specified index.</returns>
        public Token this[int index] => this.list[index];

        /// <inheritdoc/>
        public IEnumerator<Token> GetEnumerator() => this.list.GetEnumerator();

        /// <summary>
        /// Returns the content of all of the tokens in the <see cref="TokenList"/>.
        /// </summary>
        /// <returns>A <see cref="string"/> containing the content of all of the tokens in the list; or an empty string if the token list is empty.</returns>
        public override string ToString()
        {
            if (this.list.Count <= 0)
            {
                return string.Empty;
            }

            var builder = new System.Text.StringBuilder();
            int lineNumber = this.list.First().LineNumber;

            foreach (Token token in this.list)
            {
                if (token.LineNumber > lineNumber)
                {
                    builder.Append(Environment.NewLine);
                    lineNumber = token.LineNumber;
                }

                builder.Append(token.ToString());
            }

            return builder.ToString();
        }

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
