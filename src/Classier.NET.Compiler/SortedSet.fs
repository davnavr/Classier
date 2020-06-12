namespace Classier.NET.Compiler

open System.Collections.Immutable

/// Provides helper functions for interacting with sorted sets.
module SortedSet =
    let add item (set: ImmutableSortedSet<_>) =
        if set.Contains(item)
        then None
        else set.Add item |> Some

    let getEquivalent item (set: ImmutableSortedSet<_>) =
        let mutable value = item
        if set.TryGetValue(item, &value)
        then Some value
        else None
