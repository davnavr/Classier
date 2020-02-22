/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing
{
    using System;
    using System.Collections;
    using System.Collections.Generic;

    /// <summary>
    /// Represents the default collection of the token definitions.
    /// This class cannot be inherited.
    /// </summary>
    [Obsolete]
    internal sealed class DefaultTokenCollection : IEnumerable<ITokenDefinition>
    {
        /// <inheritdoc/>
        public IEnumerator<ITokenDefinition> GetEnumerator() // TODO: Make this a static property or field instead.
        {
            yield return new RegexTokenDefinition(TokenType.AccessModifier, "public|private");
            yield return new RegexTokenDefinition(TokenType.BinaryLiteral, "-?0[bB][01]([01_]*[01])?");
            yield return new RegexTokenDefinition(TokenType.CloseCurlyBracket, "}");
            yield return new RegexTokenDefinition(TokenType.CloseParen, "\\)");
            yield return new RegexTokenDefinition(TokenType.Comment, "(\\/\\/.*)|(\\/\\*[\\s\\S]*?\\*\\/)");
            yield return new RegexTokenDefinition(TokenType.Delimiter, "\\.");
            yield return new RegexTokenDefinition(TokenType.HexLiteral, "-?0[xX][0-9a-fA-F]([0-9a-fA-F_]*[0-9a-fA-F])?");

            yield return new RegexTokenDefinition(TokenType.Keyword, "class|extends|implements|interface|namespace");
            yield return new RegexTokenDefinition(TokenType.Keyword, "abstract|get|mutable|override|set|var|virtual|void");
            yield return new RegexTokenDefinition(TokenType.Keyword, "catch|finally|if|new|null|super|this|try|using|while");
            yield return new RegexTokenDefinition(TokenType.NumberLiteral, "-?([0-9]([0-9_]*[0-9])?)|([0-9]?\\.[0-9]([0-9_]*[0-9])?)");
            yield return new RegexTokenDefinition(TokenType.OpenCurlyBracket, "{");
            yield return new RegexTokenDefinition(TokenType.OpenParen, "\\(");
            yield return new RegexTokenDefinition(TokenType.Operator, "\\+|-|\\*|\\/|%|=|<|>"); // TODO: Add other operators.
            yield return new RegexTokenDefinition(TokenType.Operator, "(\\|\\|)|&&"); // Boolean operators
            yield return new RegexTokenDefinition(TokenType.ParamSeparator, ",");
            yield return new RegexTokenDefinition(TokenType.PreprocessorDir, "#[a-z]+");
            yield return new RegexTokenDefinition(TokenType.StatementEnd, ";");
            yield return new RegexTokenDefinition(TokenType.StringLiteral, "\".*\"");
            yield return new RegexTokenDefinition(TokenType.Whitespace, "\\s+");

            // Lower priority, the lexer will prioritize the definitions above.
            yield return new RegexTokenDefinition(TokenType.Identifier, "[a-zA-Z][a-zA-Z0-9]*");
        }

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
