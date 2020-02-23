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

        internal TextLineCollection(Func<TextReader> source)
        {
            this.source = source ?? throw new ArgumentNullException(nameof(source));
        }

        public IEnumerator<string> GetEnumerator()
        {
            // TODO: Get all of the lines.
            using (TextReader reader = this.source.Invoke())
            {
            }

            throw new NotImplementedException();
        }

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
