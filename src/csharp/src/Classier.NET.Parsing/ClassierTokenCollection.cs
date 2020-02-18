/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using ClassierTokenDefinition = ITokenDefinition<ClassierTokenType>;

    /// <summary>
    /// Represents a collection of the tokens in the Classier language.
    /// This class cannot be inherited.
    /// </summary>
    public sealed class ClassierTokenCollection : IEnumerable<ClassierTokenDefinition>
    {
        /// <inheritdoc/>
        public IEnumerator<ClassierTokenDefinition> GetEnumerator()
        {
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.AccessModifier, "public|private");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.BinaryLiteral, "-?0[bB][01]([01_]*[01])?");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.CloseCurlyBracket, "}");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.CloseParen, "\\)");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.Delimiter, "\\.");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.HexLiteral, "-?0[xX][0-9a-fA-F]([0-9a-fA-F_]*[0-9a-fA-F])?");
            //// TODO: Add other keywords.
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.Keyword, "class|extends|implements|interface|namespace");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.Keyword, "abstract|get|mutable|set|var|virtual|void");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.Keyword, "if|new|super|using|while");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.NumberLiteral, "-?([0-9]([0-9_]*[0-9])?)|([0-9]?\\.[0-9]([0-9_]*[0-9])?)"); // TODO: Separate into integer and float literals?
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.OpenCurlyBracket, "{");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.OpenParen, "\\(");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.Operator, "\\+|-|\\*|\\/|%"); // TODO: Add other operators.
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.ParamSeparator, ",");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.PreprocessorDir, "#[a-z]+");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.SingleLineComment, "\\/\\/.*");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.StatementEnd, ";");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.StringLiteral, "\".*\"");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.Whitespace, "\\s+");

            // Lower priority, the lexer will prioritize the definitions above.
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.Identifier, "[a-zA-Z][a-zA-Z0-9]*");
        }

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
