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
    using System.Text;
    using System.Text.RegularExpressions;

    /// <summary>
    /// Represents a collection of lines in a <see cref="TextReader"/>.
    /// This class cannot be inherited.
    /// </summary>
    internal sealed class TextLineCollection : IEnumerable<string>
    {
        private static readonly Regex NewlineRegex = new Regex("(\\r.|\\n.|\\r\\n)$");

        private readonly Func<TextReader> source;

        /// <summary>
        /// Initializes a new instance of the <see cref="TextLineCollection"/> class.
        /// </summary>
        /// <param name="source">Provides the <see cref="TextReader"/> containing the lines in this collection.</param>
        internal TextLineCollection(Func<TextReader> source)
        {
            this.source = source ?? throw new ArgumentNullException(nameof(source));
        }

        /// <summary>
        /// Returns an enumerator that iterates through the lines of the <see cref="TextReader"/>.
        /// </summary>
        /// <returns>An enumerator that iterates through the lines, including the newline characters at the end of each line.</returns>
        public IEnumerator<string> GetEnumerator()
        {
            using (TextReader reader = this.source.Invoke())
            {
                var builder = new StringBuilder();

                while (reader.Peek() >= 0)
                {
                    builder.Append((char)reader.Read());

                    int next = reader.Peek();
                    string current = builder.ToString();

                    // Either the last character has been read, or the line ends in a newline.
                    if (next < 0 || NewlineRegex.IsMatch(current))
                    {
                        yield return current;
                        builder = new StringBuilder();
                    }
                }
            }
        }

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
