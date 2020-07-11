namespace Classier.NET.Compiler.SemAnalysis

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Extern
open Classier.NET.Compiler.IR

// TODO: Make everything in this file part of the module?
type GlobalTypeSymbol =
    { Namespace: Namespace
      Type: DefinedOrExtern<AccessControl.GlobalAccess * GenType, EType> }

/// Stores the namespaces and types declared in compilation units.
type GlobalsTable =
    private
    | GlobalsTable of ImmutableSortedDictionary<Namespace, ImmutableSortedSet<GlobalTypeSymbol>>

module GlobalsTable =
    let private emptySymbols =
        let compare symbol1 symbol2 =
                  let ns =
                      compare
                          symbol1.Namespace
                          symbol2.Namespace
                  match ns with
                  | 0 ->
                      let (type1, type2) = (symbol1.Type, symbol2.Type)
                      let tname tdef =
                          match tdef with
                          | Defined (_, def) ->
                              GenType.name def
                              |> Identifier.noGenerics
                          | Extern ext ->
                              Extern.typeName ext
                              |> Identifier.noGenerics
                      let name =
                          compare
                              (tname type1)
                              (tname type2)
                      match name with
                      | 0 ->
                          match (type1, type2) with
                          | (Defined (_, def), Extern ext)
                          | (Extern ext, Defined (_, def)) ->
                              let value =
                                  match type1 with
                                  | Defined _ -> 1
                                  | _ -> -1
                              match (def, ext) with
                              | (GenModule _, EModule _) -> 0
                              | (GenModule _, _) -> value
                              | (_, EModule _) -> value * -1
                              | _ -> 0
                          | (Defined (_, def1), Defined (_, def2)) ->
                              match (def1, def2) with
                              | (GenModule _, GenModule _) -> 0
                              | (_, GenModule _) -> -1
                              | (GenModule _, _) -> 1
                              | _ -> 0
                          | (Extern ext1, Extern ext2) ->
                              match (ext1, ext2) with
                              | (EModule _, EModule _) -> 0
                              | (_, EModule _) -> -1
                              | (EModule _, _) -> 1
                              | _ -> 0
                      | _ -> name
                  | _ -> ns
        SortedSet.withComparison
            compare
            ImmutableSortedSet.Empty

    let empty = GlobalsTable ImmutableSortedDictionary.Empty

    let getNamespaces (GlobalsTable table) = table.Keys

    let getSymbols ns (GlobalsTable table) =
        match table.TryGetValue ns with
        | (true, symbols) -> symbols
        | (false, _) -> emptySymbols

    let addSymbol tsymbol globals =
        let (GlobalsTable table) = globals
        let set =
            getSymbols
                tsymbol.Namespace
                globals
        match set.TryGetValue tsymbol with
        | (true, existing) ->
            Result.Error existing
        | (false, _) ->
            table
            |> ImmSortedDict.setItem
                tsymbol.Namespace
                (SortedSet.add tsymbol set)
            |> GlobalsTable
            |> Result.Ok

    let replaceSymbol replacement globals =
        let (GlobalsTable table) = globals
        let types =
            getSymbols replacement.Namespace globals
        let added =
            match types.TryGetValue replacement with
            | (true, _) ->
                types
                |> SortedSet.remove replacement
                |> SortedSet.add replacement
            | _ ->
                types.Add replacement
        table.SetItem(replacement.Namespace, added)
        |> GlobalsTable
