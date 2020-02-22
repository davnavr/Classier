/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    using System.Text;
    using System.Text.RegularExpressions;

    /// <summary>
    /// Represents a collection of tokens originating from a <see cref="TextReader"/>.
    /// </summary>
    public class TextTokenCollection : IEnumerable<Token>
    {
        private static readonly Dictionary<TokenType, Regex> TokenDefinitionDictionary = new Dictionary<TokenType, Regex>();

        private readonly Func<TextReader> source;

        static TextTokenCollection()
        {
            TokenDefinitionDictionary.Add(TokenType.AccessModifier, new Regex("public|private"));
            TokenDefinitionDictionary.Add(TokenType.BinaryLiteral, new Regex("-?0[bB][01]([01_]*[01])?"));
            TokenDefinitionDictionary.Add(TokenType.CloseCurlyBracket, new Regex("}"));
            TokenDefinitionDictionary.Add(TokenType.CloseParen, new Regex("\\)"));
            TokenDefinitionDictionary.Add(TokenType.Comment, new Regex("(\\/\\/.*)|(\\/\\*[\\s\\S]*?\\*\\/)")); // TODO: Separate comments into two types of tokens.
            TokenDefinitionDictionary.Add(TokenType.Delimiter, new Regex("\\."));
            TokenDefinitionDictionary.Add(TokenType.HexLiteral, new Regex("-?0[xX][0-9a-fA-F]([0-9a-fA-F_]*[0-9a-fA-F])?"));
            TokenDefinitionDictionary.Add(TokenType.Identifier, new Regex("[a-zA-Z][a-zA-Z0-9]*"));
            TokenDefinitionDictionary.Add(TokenType.Keyword, new Regex(
                "class|extends|implements|interface|namespace|" +
                "abstract|get|mutable|override|set|var|virtual|void|" +
                "catch|finally|if|new|null|super|this|try|using|while"));
            TokenDefinitionDictionary.Add(TokenType.NumberLiteral, new Regex("-?([0-9]([0-9_]*[0-9])?)|([0-9]?\\.[0-9]([0-9_]*[0-9])?)"));
            TokenDefinitionDictionary.Add(TokenType.OpenCurlyBracket, new Regex("{"));
            TokenDefinitionDictionary.Add(TokenType.OpenParen, new Regex("\\("));
            TokenDefinitionDictionary.Add(TokenType.Operator, new Regex("\\+|-|\\*|\\/|%|=|<|>|(\\|\\|)|&&")); // TODO: Add other operators.
            TokenDefinitionDictionary.Add(TokenType.ParamSeparator, new Regex(","));
            TokenDefinitionDictionary.Add(TokenType.PreprocessorDir, new Regex("#[a-z]+"));
            TokenDefinitionDictionary.Add(TokenType.StatementEnd, new Regex(";"));
            TokenDefinitionDictionary.Add(TokenType.StringLiteral, new Regex("\".*\""));
            TokenDefinitionDictionary.Add(TokenType.Whitespace, new Regex("\\s+"));
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="TextTokenCollection"/> class.
        /// </summary>
        /// <param name="source">Provides the <see cref="TextReader"/> used to read the tokens.</param>
        /// <exception cref="ArgumentNullException"><paramref name="source"/> is <see langword="null"/>.</exception>
        public TextTokenCollection(Func<TextReader> source)
        {
            this.source = source ?? throw new ArgumentNullException(nameof(source));
        }

        /// <inheritdoc/>
        public IEnumerator<Token> GetEnumerator()
        {
            using (TextReader reader = this.source())
            {
                int lineNum = 0;

                while (true)
                {
                    string line = reader.ReadLine();
                    int linePos = 0;

                    // Check if the end of the file was reached.
                    if (line == null)
                    {
                        break;
                    }

                    // Find tokens from left to right, preferring longer tokens.
                    while (line.Length > 0)
                    {
                        var matches = TokenDefinitionDictionary
                            .Select(pair => new { Type = pair.Key, Match = pair.Value.Match(line) })
                            .Where(def => def.Match.Success);

                        // No matches were found in this line at all.
                        if (!matches.Any())
                        {
                            yield return new Token(line, lineNum, default);
                            line = string.Empty;
                            break;
                        }

                        int minIndex = matches.Select(def => def.Match.Index).Min();
                        var match = matches
                            .Where(def => def.Match.Index == minIndex)
                            .Aggregate((current, next) => next.Match.Length > current.Match.Length ? next : current);

                        // Unknown token in between.
                        if (minIndex > 0)
                        {
                            yield return new Token(line.Substring(0, minIndex), lineNum, default);
                            line = line.Substring(minIndex + 1);
                        }

                        yield return new Token(line.Substring(minIndex, match.Match.Length), lineNum, match.Type);
                        line = line.Substring(0, match.Match.Length);
                        linePos += match.Match.Length;
                    }

                    lineNum++;
                }
            }
        }

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
