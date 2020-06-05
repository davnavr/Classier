namespace Classier.NET.Compiler.Grammar

open System.Collections.Immutable

/// Stores the namespaces and types declared in compilation units.
type GlobalsTable =
    { Namespaces: ImmutableSortedDictionary<string list, ImmutableSortedSet<GlobalSymbol>>
      // TODO: Fix, nested types with different parents but the same name might not work here.
      Types: ImmutableSortedDictionary<Definition, TypeDef>
      (*Symbols: something *) }

module GlobalsTable =
    let empty =
        { Namespaces = ImmutableSortedDictionary.Empty
          Types = ImmutableSortedDictionary.Empty }

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

    let addType typeDef table =
        invalidOp "not implemented"
