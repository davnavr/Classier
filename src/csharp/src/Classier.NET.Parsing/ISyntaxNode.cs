/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing
{
    using System.Collections.Generic;

    /// <summary>
    /// Represents a node in the syntax tree.
    /// </summary>
    public interface ISyntaxNode
    {
        /// <summary>
        /// Gets the tokens that make up this node.
        /// </summary>
        IReadOnlyList<Token> Tokens { get; }
    }
}
