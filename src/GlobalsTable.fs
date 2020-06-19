namespace Classier.NET.Compiler

open System.Collections.Immutable
open Classier.NET.Compiler.Extern
open Classier.NET.Compiler.GlobalType
open Classier.NET.Compiler.Grammar

/// Stores the namespaces and types declared in compilation units.
type GlobalsTable = GlobalsTable of ImmutableSortedDictionary<OptIdentifier, ImmutableSortedSet<GlobalTypeSymbol>>

module GlobalsTable =
    let private emptySymbols =
        let compareType one two =
            match compare (getName one) (getName two) with
            | 0 ->
                match (one, two) with
                | (DefinedType dtype, ExternType etype)
                | (ExternType etype, DefinedType dtype) ->
                    match (dtype, etype.Kind) with
                    | (Module _, ExternTypeKind.Module) -> 0
                    | (_, ExternTypeKind.Module)
                    | (Module _, _) ->
                        match one with
                        | ExternType _ -> 1
                        | _ -> -1
                    | _ -> 0
                | _ -> 0
            | result -> result
        let typeComparer =
            { new System.Collections.Generic.IComparer<GlobalTypeSymbol> with
                  member _.Compare(one, two) =
                      match compare one.Namespace two.Namespace with
                      | 0 -> compareType one.Type two.Type
                      | result -> result }
        ImmutableSortedSet.Empty.WithComparer typeComparer

    // TODO: Add a 'bind' function or whatever it is called.

    let empty = GlobalsTable ImmutableSortedDictionary.Empty

    let getNamespaces (GlobalsTable table) = table.Keys

    let getTypes ns (GlobalsTable table) =
        let mutable types = null
        table.TryGetValue(ns, &types) |> ignore
        match types with
        | null -> emptySymbols
        | _ -> types
    
    let addType (tsymbol: GlobalTypeSymbol) globals =
        let (GlobalsTable table) = globals
        let ns = tsymbol.Namespace

        globals
        |> getTypes ns
        |> SortedSet.add tsymbol
        |> Option.map (fun types -> table.SetItem(ns, types) |> GlobalsTable)

    /// Adds a namespace to the symbol table.
    let addNamespace ns table =
        let (GlobalsTable namespaces) = table
        namespaces.SetItem(ns, emptySymbols) |> GlobalsTable
