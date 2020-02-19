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
    public class TextTokenCollection : IEnumerable<Token>
    {
        private static readonly List<ITokenDefinition> DefaultTokenDefinitions = new DefaultTokenCollection().ToList();

        private readonly Func<TextReader> source;

        private readonly List<ITokenDefinition> tokenDefinitions;

        /// <summary>
        /// Initializes a new instance of the <see cref="TextTokenCollection"/> class.
        /// </summary>
        /// <param name="source">Provides the <see cref="TextReader"/> used to read the tokens.</param>
        /// <exception cref="ArgumentNullException"><paramref name="source"/> is <see langword="null"/>.</exception>
        public TextTokenCollection(Func<TextReader> source)
            : this(source, DefaultTokenDefinitions)
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="TextTokenCollection"/> class with the specified token definitions.
        /// </summary>
        /// <param name="source">Provides the <see cref="TextReader"/> used to read the tokens.</param>
        /// <param name="tokenDefinitions">A collection containing the token definitions used to generate the tokens.</param>
        /// <exception cref="ArgumentNullException"><paramref name="source"/> or <paramref name="tokenDefinitions"/> is <see langword="null"/>.</exception>
        /// <exception cref="ArgumentException">The <paramref name="tokenDefinitions"/> collection is empty.</exception>
        internal TextTokenCollection(Func<TextReader> source, IEnumerable<ITokenDefinition> tokenDefinitions)
        {
            this.source = source ?? throw new ArgumentNullException(nameof(source));
            this.tokenDefinitions = tokenDefinitions.Any() ? tokenDefinitions.ToList() : throw new ArgumentException("The token definition collection cannot be empty.", nameof(tokenDefinitions));
        }

        /// <inheritdoc/>
        public IEnumerator<Token> GetEnumerator()
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
                        var matches = this.tokenDefinitions
                            .Select(def => new { Definition = def, Length = def.GetTokenLength(line) })
                            .Where(pair => pair.Length > 0);

                        //// TODO: Should unknown token only extend until we find another match? Make method on definitions that also return the index of the match and pick the definition with the lowest index?
                        // Gets the token definition with the longest match, and the length of the match.
                        var match = matches.Any() ? matches.Aggregate((current, next) => next.Length > current.Length ? next : current) : new { Definition = (ITokenDefinition)new UnknownTokenDefinition(), line.Length };

                        yield return new Token(line.Substring(0, match.Length), match.Definition); // TODO: Determine whether the line number and position should be included.
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
