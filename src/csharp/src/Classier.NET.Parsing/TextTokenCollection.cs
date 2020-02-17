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

    /// <summary>
    /// Represents a collection of tokens originating from a <see cref="TextReader"/>.
    /// </summary>
    /// <typeparam name="T">The type of the token.</typeparam>
    public class TextTokenCollection<T> : IEnumerable<Token<T>>
    {
        private readonly Func<TextReader> source;

        private readonly List<ITokenDefinition<T>> tokenDefinitions;

        /// <summary>
        /// Initializes a new instance of the <see cref="TextTokenCollection{T}"/> class.
        /// </summary>
        /// <param name="source">Provides the <see cref="TextReader"/> used to read the tokens.</param>
        /// <param name="tokenDefinitions">A collection containing the token definitions used to generate the tokens.</param>
        /// <exception cref="ArgumentNullException"><paramref name="source"/> or<paramref name="tokenDefinitions"/> is <see langword="null"/>.</exception>
        public TextTokenCollection(Func<TextReader> source, IEnumerable<ITokenDefinition<T>> tokenDefinitions)
        {
            this.source = source ?? throw new ArgumentNullException(nameof(source));
            this.tokenDefinitions = tokenDefinitions?.ToList() ?? throw new ArgumentNullException(nameof(tokenDefinitions)); // TODO: Check for any null elements.
        }

        /// <inheritdoc/>
        public IEnumerator<Token<T>> GetEnumerator()
        {
            int lineNum = 0;

            using (TextReader reader = this.source())
            {
                while (true)
                {
                    string line = reader.ReadLine();
                    int linePos = 0;

                    // Check if the end of the file was reached.
                    if (line == null)
                    {
                        break;
                    }

                    // Find tokens from left to right, preferring longer tokens.
                    while (line.Length > 0)
                    {
                        // Gets the token definition with the longest match, and the length of the match.
                        var match = this.tokenDefinitions
                            .Select(def => new { Definition = def, Length = def.GetTokenLength(line) })
                            .Where(pair => pair.Length > 0)
                            .Aggregate((current, next) => next.Length > current.Length ? next : current);

                        // Checks if no token definition found a match.
                        match = match ?? new { Definition = (ITokenDefinition<T>)new UnknownTokenDefinition<T>(), line.Length };

                        yield return new Token<T>(line.Substring(0, match.Length), match.Definition); // TODO: Determine whether the line number and position should be included.
                        line = line.Substring(match.Length);
                        linePos += match.Length;
                    }

                    lineNum++;
                }
            }
        }

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
