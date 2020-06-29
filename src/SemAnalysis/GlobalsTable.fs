namespace Classier.NET.Compiler.SemAnalysis

open System.Collections.Generic
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
        { new IComparer<GlobalTypeSymbol> with
              member _.Compare(symbol1, symbol2) =
                  let ns =
                      compare
                          symbol1.Namespace
                          symbol2.Namespace
                  match ns with
                  | 0 ->
                      let (type1, type2) = (symbol1.Type, symbol2.Type)
                      let tname tdef =
                          match tdef with
                          | DefinedType (_, def) -> GenType.name def
                          | ExternType ext -> EType.name ext
                      let name =
                          compare
                              (tname type1)
                              (tname type2)
                      match name with
                      | 0 ->
                          match (type1, type2) with
                          | (DefinedType (_, def), ExternType ext)
                          | (ExternType ext, DefinedType (_, def)) ->
                              let value =
                                  match type1 with
                                  | DefinedType _ -> 1
                                  | _ -> -1
                              match (def, ext) with
                              | (GenModule _, EModule _) -> 0
                              | (GenModule _, _) -> value
                              | (_, EModule _) -> value * -1
                              | _ -> 0
                          | (DefinedType (_, def1), DefinedType (_, def2)) ->
                              match (def1, def2) with
                              | (GenModule _, GenModule _) -> 0
                              | (_, GenModule _) -> -1
                              | (GenModule _, _) -> 1
                              | _ -> 0
                          | (ExternType ext1, ExternType ext2) ->
                              match (ext1, ext2) with
                              | (EModule _, EModule _) -> 0
                              | (_, EModule _) -> -1
                              | (EModule _, _) -> 1
                              | _ -> 0
                      | _ -> name
                  | _ -> ns }
        |> ImmutableSortedSet.Empty.WithComparer

    let empty = GlobalsTable ImmutableSortedDictionary.Empty

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
        |> Option.map
            (fun types ->
                table.SetItem(tsymbol.Namespace, types) |> GlobalsTable)
