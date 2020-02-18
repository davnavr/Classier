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
        /// The token is a binary literal.
        /// </summary>
        BinaryLiteral,

        /// <summary>
        /// The token is a closing curly bracket.
        /// </summary>
        CloseCurlyBracket,

        /// <summary>
        /// The token is a closing parenthesis.
        /// </summary>
        CloseParen,

        /// <summary>
        /// The token is a comment.
        /// </summary>
        Comment,

        /// <summary>
        /// The token is a hexadecimal literal.
        /// </summary>
        HexLiteral,

        /// <summary>
        /// The token represents the name of a variable, class, method, namespace, etc.
        /// </summary>
        Identifier,

        /// <summary>
        /// The token is a keyword.
        /// </summary>
        Keyword,

        /// <summary>
        /// The token is an integer (base 10) literal.
        /// </summary>
        NumberLiteral,

        /// <summary>
        /// The token is an opening curly bracket.
        /// </summary>
        OpenCurlyBracket,

        /// <summary>
        /// The token is a opening parenthesis.
        /// </summary>
        OpenParen,

        /// <summary>
        /// The token is an operator.
        /// </summary>
        Operator,

        /// <summary>
        /// The token separates method parameters (a comma).
        /// </summary>
        ParamSeparator,

        /// <summary>
        /// The token is a preprocessor directive.
        /// </summary>
        PreprocessorDir,

        /// <summary>
        /// The token indicates the end of a statement (a semicolon).
        /// </summary>
        StatementEnd,

        /// <summary>
        /// The token is a string.
        /// </summary>
        StringLiteral,

        /// <summary>
        /// The token separates the name of a namespace and the name of a type (a period).
        /// </summary>
        TypeDelimiter,

        /// <summary>
        /// The token represents whitespace.
        /// </summary>
        Whitespace,
    }
}
