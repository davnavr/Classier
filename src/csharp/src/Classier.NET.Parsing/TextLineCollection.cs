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

    /// <summary>
    /// Represents a collection of lines in a <see cref="TextReader"/>.
    /// This class cannot be inherited.
    /// </summary>
    internal sealed class TextLineCollection : IEnumerable<string>
    {
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
            // TODO: Get all of the lines by reading the characters one by one.
            using (TextReader reader = this.source.Invoke())
            {
            }

            throw new NotImplementedException();
        }

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
