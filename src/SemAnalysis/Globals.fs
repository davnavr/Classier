[<RequireQualifiedAccess>]
module Classier.NET.Compiler.SemAnalysis.Globals

open System.Collections.Immutable

open Classier.NET.Compiler

open Classier.NET.Compiler.IR

type Symbol<'Table> =
    | NamespaceSymbol of IdentifierStr * 'Table
    | TypeSymbol of GenGlobalDecl

type Table =
    private
    | Table of ImmutableSortedSet<Symbol<Table>>

type Symbol = Symbol<Table>

let emptyTable =
    SortedSet.withComparer
        (fun s1 s2 ->
            match (s1, s2) with
            | (NamespaceSymbol(ns1, _), NamespaceSymbol(ns2, _)) ->
                compare ns1 ns2
            | (NamespaceSymbol(ns, _), TypeSymbol t)
            | (TypeSymbol t, NamespaceSymbol(ns, _)) ->
                let cname =
                    Identifier.ncompare (Identifier.ofStr ns) (GenType.gname t)
                match (cname, s1) with
                | (0, _) -> 0
                | (_, NamespaceSymbol _) -> 1
                | (_, TypeSymbol _) -> -1
            | (TypeSymbol t1, TypeSymbol t2) ->
                match (t1, t2) with
                | (GenGlobalModule mdle1, GenGlobalModule mdle2) ->
                    compare mdle1.ModuleName mdle2.ModuleName
                | (GenGlobalModule _, _)
                | (_, GenGlobalModule _) ->
                    match t1 with
                    | GenGlobalModule _ -> 1
                    | _ -> -1
                | _ ->
                    Identifier.ncompare (GenType.gname t1) (GenType.gname t2))
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
