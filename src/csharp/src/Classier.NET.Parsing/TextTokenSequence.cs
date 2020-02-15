/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;

    /// <summary>
    /// Represents a sequence of tokens originating from a <see cref="TextReader"/>.
    /// </summary>
    public class TextTokenSequence
    {
        private readonly Func<TextReader> source;

        private readonly List<ITokenDefinition> tokenDefinitions;

        /// <summary>
        /// Initializes a new instance of the <see cref="TextTokenSequence"/> class.
        /// </summary>
        /// <param name="source">Provides the <see cref="TextReader"/> used to read the tokens.</param>
        /// <param name="tokenDefinitions">A collection containing the token definitions used to generate the tokens.</param>
        /// <exception cref="ArgumentNullException"><paramref name="source"/> or<paramref name="tokenDefinitions"/> is <see langword="null"/>.</exception>
        public TextTokenSequence(Func<TextReader> source, IEnumerable<ITokenDefinition> tokenDefinitions)
        {
            this.source = source ?? throw new ArgumentNullException(nameof(source));
            this.tokenDefinitions = tokenDefinitions.ToList();
        }
    }
}
