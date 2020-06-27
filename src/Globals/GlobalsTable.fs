namespace Classier.NET.Compiler.Globals

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Globals.GlobalType
open Classier.NET.Compiler.Grammar

/// Stores the namespaces and types declared in compilation units.
type GlobalsTable<'Namespace> = GlobalsTable of ImmutableSortedDictionary<'Namespace, ImmutableSortedSet<GlobalTypeSymbol>>

[<RequireQualifiedAccess>]
module GlobalsTable =
    type GlobalsTable = GlobalsTable<Namespace>

    let private emptySymbols =
        let compareType one two =
            match compare (getName one) (getName two) with
            | 0 ->
                match (one, two) with
                | (DefinedType dtype, ExternType etype)
                | (ExternType etype, DefinedType dtype) ->
                    match (dtype, etype) with
                    | (Module _, EModule _) -> 0
                    | (_, EModule _)
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

    let empty: GlobalsTable = GlobalsTable ImmutableSortedDictionary.Empty

    let getNamespaces (GlobalsTable table) = table.Keys

    let getTypes ns (GlobalsTable table) =
        match table.TryGetValue ns with
        | (true, symbols) -> symbols
        | (false, _) -> emptySymbols

    let addType (tsymbol: GlobalTypeSymbol) globals =
        let (GlobalsTable table) = globals
        globals
        |> getTypes tsymbol.Namespace
        |> SortedSet.tryAdd tsymbol
        |> Option.map (fun types -> table.SetItem(tsymbol.Namespace, types) |> GlobalsTable)

    /// Adds a namespace to the symbol table.
    let addNamespace ns table =
        let (GlobalsTable namespaces) = table
        namespaces.SetItem(ns, emptySymbols) |> GlobalsTable
