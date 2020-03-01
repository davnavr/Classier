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
    using System.Text.RegularExpressions;

    /// <summary>
    /// Represents a collection of syntax nodes parsed from tokens.
    /// This class cannot be inherited.
    /// </summary>
    internal sealed class ParsedTokenCollection : IEnumerable<ISyntaxNode>
    {
        private readonly List<string> symbols;

        private readonly IEnumerable<Token> tokens;

        static ParsedTokenCollection()
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="ParsedTokenCollection"/> class.
        /// </summary>
        /// <param name="tokens">The tokens to parse.</param>
        /// <param name="symbols">A collection containing the conditional compilation symbols.</param>
        /// <exception cref="ArgumentNullException"><paramref name="symbols"/> or <paramref name="tokens"/> is <see langword="null"/>.</exception>
        internal ParsedTokenCollection(IEnumerable<Token> tokens, IEnumerable<string> symbols)
        {
            this.symbols = symbols?.ToList() ?? throw new ArgumentNullException(nameof(symbols));
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
