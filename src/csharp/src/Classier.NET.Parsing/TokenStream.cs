/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Linq;

    /// <summary>
    /// Wraps a collection of tokens and provides method that ensure that retrieved tokens are valid.
    /// This class cannot be inherited and is mutable.
    /// </summary>
    internal sealed class TokenStream
    {
        private readonly Queue<Token> queue;

        /// <summary>
        /// Initializes a new instance of the <see cref="TokenStream"/> class.
        /// </summary>
        /// <param name="tokens">The tokens in the <see cref="TokenStream"/>.</param>
        /// <exception cref="ArgumentNullException"><paramref name="tokens"/> is <see langword="null"/>.</exception>
        internal TokenStream(IEnumerable<Token> tokens)
        {
            this.queue = new Queue<Token>(tokens ?? throw new ArgumentNullException(nameof(tokens)));
        }

        /// <summary>
        /// Gets the next tokens from the stream, ensuring they are of the specified type.
        /// </summary>
        /// <param name="expectedType">The type of the tokens to retrieve.</param>
        /// <param name="minimumCount">The minimum number of tokens to retrieve.</param>
        /// <returns>The tokens of the specified type that were retrieved.</returns>
        /// <exception cref="ArgumentException">The <paramref name="minimumCount"/> is less than zero.</exception>
        /// <exception cref="InvalidOperationException">The number of tokens retrieved is less than the <paramref name="minimumCount"/>.</exception>
        internal IEnumerable<Token> NextTokens(TokenType expectedType, int minimumCount)
        {
            if (minimumCount < 0)
            {
                throw new ArgumentException("The minimum number of tokens must not be negative.", nameof(minimumCount));
            }

            int actual = 0;

            while (this.queue.Count > 0 && this.queue.Peek().TokenType == expectedType)
            {
                yield return this.queue.Dequeue();
                actual++;
            }

            if (actual < minimumCount)
            {
                throw new InvalidOperationException($"Expected at least {minimumCount} {expectedType} tokens, but {actual} were retrieved.");
            }
        }

        /// <summary>
        /// Retrieves the next tokens as long as there are tokens left in the stream and the specified condition is satisfied.
        /// </summary>
        /// <param name="condition">The function used to determine whether the next token should be retrieved.</param>
        /// <returns>The tokens that matched the <paramref name="condition"/>.</returns>
        internal IEnumerable<Token> NextTokens(Func<Token, bool> condition)
        {
            while (this.queue.Count > 0 && condition.Invoke(this.queue.Peek()))
            {
                yield return this.queue.Dequeue();
            }
        }

        /// <summary>
        /// Gets the next token from the stream and ensures it is of the expected type.
        /// </summary>
        /// <param name="expectedType">The expected type of the next token.</param>
        /// <returns>The next token.</returns>
        /// <exception cref="InvalidOperationException">The type of the next token does not match the expected type.</exception>
        internal Token NextToken(TokenType expectedType)
        {
            Token next = this.queue.Peek();
            return next.TokenType == expectedType ? this.queue.Dequeue() : throw new InvalidOperationException($"A {expectedType} token was expected, but a {next.TokenType} token was retrieved instead.");
        }
    }
}
