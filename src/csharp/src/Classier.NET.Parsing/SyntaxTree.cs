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
    /// Represents a single Classier source code file.
    /// </summary>
    public class SyntaxTree : IReadOnlyCollection<ISyntaxNode>
    {
        private readonly List<Token> tokenList;

        private readonly List<string> symbols;

        private readonly Lazy<IEnumerator<ISyntaxNode>> nodeEnumerator;

        /// <summary>
        /// Initializes a new instance of the <see cref="SyntaxTree"/> class from the specfiied tokens.
        /// </summary>
        /// <param name="tokens">The tokens in the source code file.</param>
        /// <exception cref="ArgumentNullException"><paramref name="tokens"/> is <see langword="null"/>.</exception>
        public SyntaxTree(IEnumerable<Token> tokens)
            : this(tokens, Enumerable.Empty<string>())
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="SyntaxTree"/> class from the specfiied tokens and conditional compilation symbols.
        /// </summary>
        /// <param name="tokens">The tokens in the source code file.</param>
        /// <param name="symbols">An array containing the conditional compilation symbols.</param>
        /// <exception cref="ArgumentNullException"><paramref name="tokens"/> or <paramref name="symbols"/> is <see langword="null"/>.</exception>
        public SyntaxTree(IEnumerable<Token> tokens, params string[] symbols)
            : this(tokens, symbols?.AsEnumerable())
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="SyntaxTree"/> class from the specfiied tokens and conditional compilation symbols.
        /// </summary>
        /// <param name="tokens">The tokens in the source code file.</param>
        /// <param name="symbols">A collection containing the conditional compilation symbols.</param>
        /// <exception cref="ArgumentNullException"><paramref name="tokens"/> or <paramref name="symbols"/> is <see langword="null"/>.</exception>
        public SyntaxTree(IEnumerable<Token> tokens, IEnumerable<string> symbols)
        {
            this.nodeEnumerator = new Lazy<IEnumerator<ISyntaxNode>>(this.GetEnumerator);
            this.tokenList = tokens?.ToList() ?? throw new ArgumentNullException(nameof(tokens));
            this.symbols = symbols?.ToList() ?? throw new ArgumentNullException(nameof(symbols));
            throw new NotImplementedException();
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="SyntaxTree"/> class from a file with the specified encoding and conditional compilation symbols.
        /// </summary>
        /// <param name="path">The path to the source code file.</param>
        /// <param name="encoding">The encoding of the source code file.</param>
        /// <param name="symbols">A collection containing the conditional compilation symbols.</param>
        /// <exception cref="ArgumentNullException"><paramref name="path"/>, <paramref name="encoding"/>, or <paramref name="symbols"/> is <see langword="null"/>.</exception>
        public SyntaxTree(string path, System.Text.Encoding encoding, IEnumerable<string> symbols)
            : this(new TextTokenCollection(() => new System.IO.StreamReader(path, encoding)), symbols)
        {
        }

        /// <summary>
        /// Gets the total number of syntax nodes in this syntax tree.
        /// </summary>
        public int Count => throw new NotImplementedException();

        /// <inheritdoc/>
        public IEnumerator<ISyntaxNode> GetEnumerator()
        {
            if (this.nodeEnumerator.IsValueCreated)
            {
                return this.nodeEnumerator.Value;
            }

            throw new NotImplementedException();
        }

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
