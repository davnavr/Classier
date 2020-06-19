namespace Classier.NET.Compiler

open System.Collections.Immutable

/// Provides helper functions for interacting with sorted sets.
module SortedSet =
    let (|Contains|_|) item (set: ImmutableSortedSet<_>) =
        if set.Contains(item)
        then Some ()
        else None

    let add item (set: ImmutableSortedSet<_>) =
        match set with
        | Contains item -> None
        | _ -> set.Add item |> Some

    let getEquivalent item (set: ImmutableSortedSet<_>) =
        let mutable value = item
        if set.TryGetValue(item, &value)
        then Some value
        else None
