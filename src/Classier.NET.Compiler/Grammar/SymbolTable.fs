namespace Classier.NET.Compiler.Grammar

open System.Collections.Immutable
open FParsec

// TODO: Symbol table should only be filled with classes, modules, interfaces, methods, functions, etc. & should ignore local vars.
type SymbolTable =
    { Namespaces: ImmutableSortedDictionary<string list, ImmutableSortedSet<ResolvedSymbol>>
      Types: ImmutableSortedDictionary<Definition, TypeDef>
      (*Symbols: something *) }

module SymbolTable =
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
        let def = typeDef.Definition
        if table.Types.ContainsKey(def)
        then None
        else Some { table with Types = table.Types.Add(def, typeDef) }
