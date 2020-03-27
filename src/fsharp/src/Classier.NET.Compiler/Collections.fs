// Copyright (c) 2020, David Navarro. All rights reserved.
// Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.

namespace Classier.NET.Compiler.Collections

open System.Collections.Generic

type Item<'T> =
    /// Represents an item in a collection.
    | Item of 'T
    /// Indicates that the end of the collection has been reached.
    | End

type Cursor<'T> private (e: IEnumerator<'T>, pos: int) =
    let hasVal = e.MoveNext()
    let item =
        if hasVal
        then Item e.Current
        else End
    let next = lazy Cursor(e, pos + 1)
    
    new(c: IEnumerable<'T>) = Cursor(c.GetEnumerator(), 0)

    member _this.Item = item
    member _this.Index = pos
    member _this.ReachedEnd = not hasVal
    member this.Next =
        if hasVal then
            next.Value
        else
            e.Dispose() |> ignore
            this
