// Copyright (c) 2020, David Navarro. All rights reserved.
// Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.

namespace Classier.NET.Compiler.Collections

open System.Collections.Generic

type Item<'T> =
    | Item of 'T
    | End

type Cursor<'T> private (e: IEnumerator<'T>) =
    let hasVal = e.MoveNext()
    let item =
        if hasVal then
            Item e.Current
        else
            End
    let next = lazy Cursor(e)

    new(c: IEnumerable<'T>) =
        Cursor(c.GetEnumerator())

    member this.Item = item
    member this.Next = next.Value
