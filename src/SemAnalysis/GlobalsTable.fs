namespace Classier.NET.Compiler.SemAnalysis

open System.Collections.Immutable

open Classier.NET.Compiler
open Classier.NET.Compiler.Identifier

open Classier.NET.Compiler.Extern
open Classier.NET.Compiler.IR

[<RequireQualifiedAccess>]
module GlobalsTable =
    type private GlobalType = DefinedOrExtern<GenGlobalType, EGlobalType>

    type Table =
        private
        | GlobalsTable of ImmutableSortedDictionary<Namespace, ImmutableSortedSet<GlobalType>>

    let private emptySymbols =
        SortedSet.withComparer
            (fun type1 type2 ->
                let tname =
                    function
                    | Defined def ->
                        GenType.gname def |> Identifier.noGenerics
                    | Extern ext ->
                        EType.gname ext |> Identifier.noGenerics
                let cname =
                    compare
                        (tname type1)
                        (tname type2)
                match cname with
                | 0 ->
                    match (type1, type2) with
                    | (Defined def1, Defined def2) ->
                        match (def1, def2) with
                        | (GenGlobalModule _, GenGlobalModule _) -> 0
                        | (_, GenGlobalModule _) -> -1
                        | (GenGlobalModule _, _) -> 1
                        | _ -> 0
                    | (Extern ext1, Extern ext2) ->
                        match (ext1, ext2) with
                        | (EGlobalModule _, EGlobalModule _) -> 0
                        | (_, EGlobalModule _) -> -1
                        | (EGlobalModule _, _) -> 1
                        | _ -> 0
                | _ -> cname)
            ImmutableSortedSet.Empty

    let empty = GlobalsTable ImmutableSortedDictionary.Empty

    let types ns (GlobalsTable table) =
        ImmSortedDict.tryGetValue
            ns
            id
            (fun () -> emptySymbols)
            table

    let addSymbol tdef ns globals =
        let (GlobalsTable table) = globals
        let nstypes =
            types ns globals
        SortedSet.tryGetValue
            tdef
            Result.Error
            (fun() ->
                ImmSortedDict.setItem
                    ns
                    (SortedSet.add tdef nstypes)
                    table
                |> GlobalsTable
                |> Result.Ok)
            nstypes

type GlobalsTable = GlobalsTable.Table
