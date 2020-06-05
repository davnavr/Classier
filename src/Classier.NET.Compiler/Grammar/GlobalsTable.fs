namespace Classier.NET.Compiler.Grammar

open System.Collections.Immutable

/// Stores the namespaces and types declared in compilation units.
type GlobalsTable =
    { Namespaces: ImmutableSortedDictionary<string list, ImmutableSortedSet<GlobalSymbol>>
      (*Symbols: something *) }

module GlobalsTable =
    let empty = { Namespaces = ImmutableSortedDictionary.Empty }

    /// Adds a namespace to the symbol table.
    let addNamespace ns table =
        let namespaces, _ =
            ns
            |> Seq.fold
                (fun (nsDict: ImmutableSortedDictionary<string list, _>, prev) name ->
                    let next = prev @ [ name ]
                    let newDict =
                        if nsDict.ContainsKey(next)
                        then nsDict
                        else nsDict.Add(next, ImmutableSortedSet.Empty)
                    (newDict, next))
                (table.Namespaces, List.empty)
        { table with Namespaces = namespaces }

    let addType typeDef ns table =
        let withNs = addNamespace ns table
        let currentTypes = withNs.Namespaces.Item ns
        // TODO: Figure out how to get the GlobalSymbol objects representing the parent namespace.
        { withNs with Namespaces = withNs.Namespaces.SetItem(ns, currentTypes.Add(GlobalSymbol.ofType typeDef [])) }
