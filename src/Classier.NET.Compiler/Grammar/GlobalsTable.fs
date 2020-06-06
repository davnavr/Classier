namespace Classier.NET.Compiler.Grammar

open System.Collections.Immutable

/// Stores the namespaces and types declared in compilation units.
type GlobalsTable = GlobalsTable of ImmutableSortedDictionary<string list, ImmutableSortedSet<GlobalTypeSymbol>>

module GlobalsTable =
    let empty = GlobalsTable ImmutableSortedDictionary.Empty

    let getTypes ns (GlobalsTable table) =
        let mutable types = null
        table.TryGetValue(ns, &types) |> ignore
        match types with
        | null -> ImmutableSortedSet.Empty
        | _ -> types
    
    let addTypes types ns table =
        let (GlobalsTable namespaces) = table
        namespaces.SetItem(ns, (getTypes ns table).Union(types))
        |> GlobalsTable

    /// Adds a namespace to the symbol table.
    let addNamespace ns = addTypes List.empty ns

