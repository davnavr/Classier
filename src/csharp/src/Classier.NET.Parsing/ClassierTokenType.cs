/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing
{
    /// <summary>
    /// Represents the type of a token in the Classier language.
    /// </summary>
    public enum ClassierTokenType
    {
        /// <summary>
        /// The token is of an unknown type.
        /// </summary>
        Unknown,

        /// <summary>
        /// The token is a closing parenthesis.
        /// </summary>
        CloseParen,

        /// <summary>
        /// The token is an integer literal.
        /// </summary>
        IntegerLiteral,

        /// <summary>
        /// The token is a opening parenthesis.
        /// </summary>
        OpenParen,

        /// <summary>
        /// The token indicates the end of a statement (a semicolon).
        /// </summary>
        StatementEnd,

        /// <summary>
        /// The token is a string.
        /// </summary>
        StringLiteral,
    }
}
