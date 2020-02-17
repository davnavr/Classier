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
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.CloseCurlyBracket, "}");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.CloseParen, ")");

            // TODO: Add other keywords.
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.Keyword, "class|interface|extends|implements");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.Keyword, "abstract|virtual|mutable|var");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.Keyword, "public|private");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.OpenCurlyBracket, "{");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.OpenParen, "(");
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.Operator, "\\+|-|\\*|\\/|%"); // TODO: Add other operators.
            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.ParamSeparator, ",");

            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.StatementEnd, ";");

            yield return new RegexTokenDefinition<ClassierTokenType>(ClassierTokenType.Whitespace, "\\s+");
            throw new NotImplementedException();
        }

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
