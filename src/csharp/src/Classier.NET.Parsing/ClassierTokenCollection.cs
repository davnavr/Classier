﻿/*
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
            ////yield return new
            throw new NotImplementedException();
        }

        /// <inheritdoc/>
        IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
    }
}
