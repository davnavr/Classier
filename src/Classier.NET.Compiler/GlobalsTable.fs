namespace Classier.NET.Compiler

open System.Collections.Immutable
open Classier.NET.Compiler.Extern
open Classier.NET.Compiler.GlobalType
open Classier.NET.Compiler.Grammar

/// Stores the namespaces and types declared in compilation units.
type GlobalsTable = GlobalsTable of ImmutableSortedDictionary<string list, ImmutableSortedSet<GlobalTypeSymbol>>

module GlobalsTable =
    let private emptySymbols =
        let compareType one two =
            match compare (getName one) (getName two) with
            | 0 ->
                match (one, two) with
                | (DefinedType dtype, ExternType etype)
                | (ExternType etype, DefinedType dtype) ->
                    match (dtype.Header, etype.Kind) with
                    | (Module, ExternTypeKind.Module) -> 0
                    | (_, ExternTypeKind.Module)
                    | (Module, _) ->
                        match one with
                        | ExternType _ -> 1
                        | _ -> -1
                    | _ -> 0
                | _ -> 0
            | _ as result -> result

        { new System.Collections.Generic.IComparer<GlobalTypeSymbol> with
              member _.Compare(one, two) =
                  match compare one.Namespace two.Namespace with
                  | 0 -> compareType one.Type two.Type
                  | _ as result -> result }
        |> ImmutableSortedSet.Empty.WithComparer

    let empty = GlobalsTable ImmutableSortedDictionary.Empty

    let getTypes ns (GlobalsTable table) =
        let mutable types = null
        table.TryGetValue(ns, &types) |> ignore
        match types with
        | null -> emptySymbols
        | _ -> types
    
    let addTypes types ns table =
        let (GlobalsTable namespaces) = table
        let added = (getTypes ns table).Union(types)
        namespaces.SetItem(ns, added) |> GlobalsTable

    /// Adds a namespace to the symbol table.
    let addNamespace ns table =
        let (GlobalsTable namespaces) = table
        namespaces.SetItem(ns, emptySymbols) |> GlobalsTable
