﻿/*
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
        private readonly Func<TextReader> source;

        private readonly List<ITokenDefinition> tokenDefinitions;

        /// <summary>
        /// Initializes a new instance of the <see cref="TextTokenCollection"/> class.
        /// </summary>
        /// <param name="source">Provides the <see cref="TextReader"/> used to read the tokens.</param>
        /// <param name="tokenDefinitions">A collection containing the token definitions used to generate the tokens.</param>
        /// <exception cref="ArgumentNullException"><paramref name="source"/> or<paramref name="tokenDefinitions"/> is <see langword="null"/>.</exception>
        public TextTokenCollection(Func<TextReader> source, IEnumerable<ITokenDefinition> tokenDefinitions)
        {
            this.source = source ?? throw new ArgumentNullException(nameof(source));
            this.tokenDefinitions = tokenDefinitions.ToList();
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
                        // Gets the token definition with the longest match, and the length of the match.
                        var match = this.tokenDefinitions
                            .Select(def => new { Definition = def, Length = def.GetTokenLength(line) })
                            .Where(pair => pair.Length > 0)
                            .Aggregate((current, next) => next.Length > current.Length ? next : current);

                        //// TODO: Make an unknown token definition.
                        //// yield return new Token();
                        line = line.Substring(match.Length);
                        linePos += match.Length;
                    }

                    lineNum++;
                }
            }

            throw new NotImplementedException();
        }

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
