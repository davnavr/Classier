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
    /// Represents a collection of the tokens in the Classier language.
    /// This class cannot be inherited.
    /// </summary>
    public sealed class ClassierTokenCollection : IEnumerable<ITokenDefinition>
    {
        /// <inheritdoc/>
        public IEnumerator<ITokenDefinition> GetEnumerator()
        {
            yield return new RegexTokenDefinition(ClassierTokenType.AccessModifier, "public|private");
            yield return new RegexTokenDefinition(ClassierTokenType.BinaryLiteral, "-?0[bB][01]([01_]*[01])?");
            yield return new RegexTokenDefinition(ClassierTokenType.CloseCurlyBracket, "}");
            yield return new RegexTokenDefinition(ClassierTokenType.CloseParen, "\\)");
            yield return new RegexTokenDefinition(ClassierTokenType.CommentEnd, "\\/\\*");
            yield return new RegexTokenDefinition(ClassierTokenType.CommentStart, "\\*\\/");
            yield return new RegexTokenDefinition(ClassierTokenType.Delimiter, "\\.");
            yield return new RegexTokenDefinition(ClassierTokenType.HexLiteral, "-?0[xX][0-9a-fA-F]([0-9a-fA-F_]*[0-9a-fA-F])?");

            yield return new RegexTokenDefinition(ClassierTokenType.Keyword, "class|extends|implements|interface|namespace");
            yield return new RegexTokenDefinition(ClassierTokenType.Keyword, "abstract|get|mutable|override|set|var|virtual|void");
            yield return new RegexTokenDefinition(ClassierTokenType.Keyword, "if|new|null|super|using|while");
            yield return new RegexTokenDefinition(ClassierTokenType.NumberLiteral, "-?([0-9]([0-9_]*[0-9])?)|([0-9]?\\.[0-9]([0-9_]*[0-9])?)");
            yield return new RegexTokenDefinition(ClassierTokenType.OpenCurlyBracket, "{");
            yield return new RegexTokenDefinition(ClassierTokenType.OpenParen, "\\(");
            yield return new RegexTokenDefinition(ClassierTokenType.Operator, "\\+|-|\\*|\\/|%|=|<|>"); // TODO: Add other operators.
            yield return new RegexTokenDefinition(ClassierTokenType.Operator, "(\\|\\|)|&&"); // Boolean operators
            yield return new RegexTokenDefinition(ClassierTokenType.ParamSeparator, ",");
            yield return new RegexTokenDefinition(ClassierTokenType.PreprocessorDir, "#[a-z]+");
            yield return new RegexTokenDefinition(ClassierTokenType.SingleLineComment, "\\/\\/.*");
            yield return new RegexTokenDefinition(ClassierTokenType.StatementEnd, ";");
            yield return new RegexTokenDefinition(ClassierTokenType.StringLiteral, "\".*\"");
            yield return new RegexTokenDefinition(ClassierTokenType.Whitespace, "\\s+");

            // Lower priority, the lexer will prioritize the definitions above.
            yield return new RegexTokenDefinition(ClassierTokenType.Identifier, "[a-zA-Z][a-zA-Z0-9]*");
        }

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
