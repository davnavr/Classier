﻿[<RequireQualifiedAccess>]
module Classier.NET.Compiler.SemAnalysis.Globals

open System.Collections.Immutable

open Classier.NET.Compiler

open Classier.NET.Compiler.Extern
open Classier.NET.Compiler.IR

type Symbol<'Table> =
    | NamespaceSymbol of IdentifierStr * 'Table
    | TypeSymbol of DefinedOrExtern<GenGlobalType, EGlobalType>

type Table =
    private
    | Table of ImmutableSortedSet<Symbol<Table>>

type Symbol = Symbol<Table>

let emptyTable =
    let tname =
        function
        | Defined def ->
            GenType.gname def |> Identifier.umap
        | Extern ext ->
            EType.gname ext |> Identifier.umap
    let cgtypes type1 type2 =
        let cname =
            compare
                (tname type1)
                (tname type2)
        match cname with
        | 0 ->
            match (type1, type2) with
            | (Defined def, Extern ext)
            | (Extern ext, Defined def) ->
                let value =
                    match type1 with
                    | Defined _ -> 1
                    | _ -> -1
                match (def, ext) with
                | (GenGlobalModule _, EGlobalModule _) -> 0
                | (GenGlobalModule _, _) -> value
                | (_, EGlobalModule _) -> value * -1
                | _ -> 0
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
        | _ -> cname
    SortedSet.withComparer
        (fun s1 s2 ->
            match (s1, s2) with
            | (NamespaceSymbol(ns1, _), NamespaceSymbol(ns2, _)) ->
                compare ns1 ns2
            | (NamespaceSymbol(ns, _), TypeSymbol t)
            | (TypeSymbol t, NamespaceSymbol(ns, _)) ->
                let value =
                    match s1 with
                    | NamespaceSymbol _ -> -1
                    | TypeSymbol _ -> 1
                let cname =
                    compare
                        (Identifier.ofStr ns)
                        (tname t)
                cname * value
            | (TypeSymbol t1, TypeSymbol t2) ->
                cgtypes t1 t2)
        ImmutableSortedSet.Empty
    |> Table

let private foldns folder fail state (Namespace ns) t =
    let rec inner nsnames acc (Table table) =
        match nsnames with
        | [] -> acc
        | name :: rest ->
            let equiv = NamespaceSymbol(name, emptyTable)
            match table.TryGetValue equiv with
            | (false, _)
            | (true, TypeSymbol _) -> fail name acc
            | (true, NamespaceSymbol (_, ntable)) ->
                inner rest (folder ntable name acc) ntable
    inner ns (state t) t

let private updatens symbols addsymbol ns ttable =
    let update table =
        Seq.fold
            (fun state nssymbol ->
                match state with
                | Result.Ok (Table build) ->
                    addsymbol
                        nssymbol
                        build
                | Result.Error _ -> state)
            (Result.Ok table)
            symbols
    let tables =
        let ncons ntable name nlist =
            (name, ntable) :: nlist
        foldns
            ncons
            (ncons emptyTable)
            (fun _ -> List.empty)
            ns
            ttable
    match tables with
    | [] -> update ttable
    | (hns, htable) :: tns ->
        let addns (Table table) symbol =
            let nssymbol = NamespaceSymbol symbol
            table
            |> SortedSet.remove nssymbol
            |> SortedSet.add nssymbol
            |> Table
        result {
            let! ntable = update htable
            return
                List.fold
                    (fun prev (nname, ntable) ->
                        nname, addns ntable prev)
                    (hns, ntable)
                    tns
                |> addns ttable
        }

let private nssymbols ns table =
    let (Table symbols) =
        foldns
            (fun t _ _ -> t)
            (fun _ _ -> emptyTable)
            id
            ns
            table
    symbols

/// Retrieves the global types in the specified namespace.
let nstypes ns table =
    Seq.choose
        (function
        | NamespaceSymbol _ -> None
        | TypeSymbol tdef -> Some tdef)
        (nssymbols ns table)

let addType tdef ns ttable =
    updatens
        [ TypeSymbol tdef ]
        (fun symbol table ->
            SortedSet.tryAdd
                symbol
                table
            |> Result.map Table)
        ns
        ttable

// TODO: Remove this function.
let hasGlobalNs ns (Table table) =
    let symbol =
        NamespaceSymbol(ns, emptyTable)
    match table with
    | SortedSet.Contains symbol -> true
    | _ -> false