namespace Classier.NET.Compiler.Grammar

open System.Collections.Immutable

/// Stores the namespaces and types declared in compilation units.
type GlobalsTable =
    { Namespaces: ImmutableSortedDictionary<string list, ImmutableSortedSet<GlobalTypeSymbol>>
      (*Symbols: something *) }

module GlobalsTable =
    let empty = { Namespaces = ImmutableSortedDictionary.Empty }

    /// Adds a namespace to the symbol table.
    let addNamespace ns table =
        if table.Namespaces.ContainsKey ns
        then table
        else { table with Namespaces = table.Namespaces.Add(ns, ImmutableSortedSet.Empty) }

    let addTypes types ns table =
        let withNs = addNamespace ns table
        let currentTypes = withNs.Namespaces.Item ns
        { withNs with Namespaces = withNs.Namespaces.SetItem(ns, currentTypes.Union(types)) }
