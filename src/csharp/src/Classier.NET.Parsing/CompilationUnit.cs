/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing
{
    using System;
    using System.Collections.Generic;
    using System.Linq;

    /// <summary>
    /// Represents a 
    /// </summary>
    public class CompilationUnit
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="CompilationUnit"/> class from the tokens.
        /// </summary>
        /// <param name="tokens"></param>
        public CompilationUnit(IEnumerable<Token> tokens)
        {
        }

        public CompilationUnit(IEnumerable<Token> tokens, params string[] symbols)
            : this(tokens, symbols.AsEnumerable())
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="CompilationUnit"/> class from the tokens and the conditional compilation symbols.
        /// </summary>
        /// <param name="tokens"></param>
        /// <param name="symbols">A collection containing the conditional compilation symbols.</param>
        public CompilationUnit(IEnumerable<Token> tokens, IEnumerable<string> symbols) // TODO: Make enumerator class that wraps an enumerator for tokens and skips whitespace tokens.
        {
        }
    }
}
