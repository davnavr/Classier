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
    /// Represents a single Classier source code file.
    /// </summary>
    public class CompilationUnit // TODO: Rename to 'SourceFile'?
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="CompilationUnit"/> class from the specfiied tokens.
        /// </summary>
        /// <param name="tokens">The tokens in the source code file.</param>
        public CompilationUnit(IEnumerable<Token> tokens)
            : this(tokens, new string[0]) // TODO: Is making an empty array faster?
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="CompilationUnit"/> class from the specfiied tokens and conditional compilation symbols.
        /// </summary>
        /// <param name="tokens">The tokens in the source code file.</param>
        /// <param name="symbols">An array containing the conditional compilation symbols.</param>
        public CompilationUnit(IEnumerable<Token> tokens, params string[] symbols)
            : this(tokens, symbols.AsEnumerable())
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="CompilationUnit"/> class from the specfiied tokens and conditional compilation symbols.
        /// </summary>
        /// <param name="tokens">The tokens in the source code file.</param>
        /// <param name="symbols">A collection containing the conditional compilation symbols.</param>
        public CompilationUnit(IEnumerable<Token> tokens, IEnumerable<string> symbols) // TODO: Make enumerator class that wraps an enumerator for tokens and skips whitespace tokens and has other functionality.
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="CompilationUnit"/> class from a file with the specified encoding and conditional compilation symbols.
        /// </summary>
        /// <param name="path">The path to the source code file.</param>
        /// <param name="encoding">The encoding of the source code file.</param>
        /// <param name="symbols">A collection containing the conditional compilation symbols.</param>
        public CompilationUnit(string path, System.Text.Encoding encoding, IEnumerable<string> symbols)
            : this(new TextTokenCollection(() => new System.IO.StreamReader(path, encoding)), symbols)
        {
        }
    }
}
