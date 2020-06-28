namespace Classier.NET.Compiler.SemAnalysis

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Globals
open Classier.NET.Compiler.ToSource

type GlobalTypeSymbol =
    { Namespace: Namespace
      Type: GlobalType<GenType> }

// TODO: How will a name conflict between a type and namespace be treated?
/// Stores the namespaces and types declared in compilation units.
type GlobalsTable =
    private
    | GlobalsTable of ImmutableSortedDictionary<Namespace, ImmutableSortedSet<GlobalTypeSymbol>>

module GlobalsTable =
    let private emptySymbols =
        let ctypes one two = invalidOp "How to compare them?"
        let typeComparer =
            { new System.Collections.Generic.IComparer<GlobalTypeSymbol> with
                    member _.Compare(one, two) =
                        match compare one.Namespace two.Namespace with
                        | 0 -> ctypes one.Type two.Type
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
