namespace Classier.NET.Compiler

open System.Collections.Immutable

/// Provides helper functions for interacting with sorted sets.
module SortedSet =
    let (|Contains|_|) item (set: ImmutableSortedSet<_>) =
        if set.Contains(item)
        then Some ()
        else None

    let length (set: ImmutableSortedSet<_>) = set.Count

    let add item (set: ImmutableSortedSet<_>) = set.Add item

    let tryAdd item (set: ImmutableSortedSet<_>) =
        match set with
        | Contains item -> None
        | _ -> set |> add item |> Some

    let remove item (set: ImmutableSortedSet<_>) = set.Remove item
