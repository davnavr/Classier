/*
 * Copyright (c) 2020, David Navarro. All rights reserved.
 * Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.
 */

namespace Classier.NET.Parsing
{
    /// <summary>
    /// Generates tokens from source code.
    /// </summary>
    public interface ITokenDefinition
    {
        /// <summary>
        /// Gets the length of a token if it was to be created, beginning from the first character in the line.
        /// </summary>
        /// <param name="line">The line containing the token.</param>
        /// <returns>The length of the token beginning from the first character in the line; or <c>0</c> if a token cannot be created.</returns>
        int GetTokenLength(string line);
    }
}
