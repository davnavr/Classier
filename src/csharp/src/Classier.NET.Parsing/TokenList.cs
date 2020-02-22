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

        public TokenList(IEnumerable<Token> tokens)
        {
            this.list = tokens?.ToList() ?? throw new ArgumentNullException(nameof(tokens));
        }

        /// <summary>
        /// Gets the number of tokens in this <see cref="TokenList"/>.
        /// </summary>
        public int Count => this.list.Count;

        public Token this[int index] => this.list[index];

        /// <inheritdoc/>
        public IEnumerator<Token> GetEnumerator() => this.list.GetEnumerator(); // TODO: Should the tokens be filtered?

        /// <summary>
        /// Returns the content of all of the tokens in the <see cref="TokenList"/>.
        /// </summary>
        /// <returns>A <see cref="string"/> containing the content of all of the tokens in the list.</returns>
        public override string ToString()
        {
            throw new NotImplementedException();
        }

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
