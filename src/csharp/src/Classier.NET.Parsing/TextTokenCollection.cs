/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    using System.Text;
    using System.Text.RegularExpressions;

    /// <summary>
    /// Represents a collection of tokens originating from a <see cref="TextReader"/>.
    /// </summary>
    public class TextTokenCollection : IEnumerable<Token>
    {
        private static readonly Dictionary<TokenType, Regex> TokenDefinitionDictionary;

        private readonly Func<TextReader> source;

        /// <summary>
        /// Initializes a new instance of the <see cref="TextTokenCollection"/> class.
        /// </summary>
        /// <param name="source">Provides the <see cref="TextReader"/> used to read the tokens.</param>
        /// <exception cref="ArgumentNullException"><paramref name="source"/> is <see langword="null"/>.</exception>
        public TextTokenCollection(Func<TextReader> source)
        {
            this.source = source ?? throw new ArgumentNullException(nameof(source));
        }

        /// <inheritdoc/>
        public IEnumerator<Token> GetEnumerator()
        {
            using (TextReader reader = this.source.Invoke())
            {
                StringBuilder builder = new StringBuilder();

                while (reader.Peek() >= 0) //// TODO: Instead of going line by line, check for longest match at index 0 instead. This may use lots of memory though, but could be avoided by consuming bytes until a match is found, and by also using a StringBuilder instead of creating a new string instance every time.
                {
                    builder.Append((char)reader.Read());
                }
            }

            throw new NotImplementedException();
        }

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
