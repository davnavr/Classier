namespace Classier.NET.Compiler.SemAnalysis

open System.Collections.Immutable

open Classier.NET.Compiler
open Classier.NET.Compiler.Identifier

open Classier.NET.Compiler.Extern
open Classier.NET.Compiler.IR

[<RequireQualifiedAccess>]
module GlobalsTable =
    type private Symbol<'Table> =
        | NamespaceSymbol of IdentifierStr * 'Table
        | TypeSymbol of DefinedOrExtern<GenGlobalType, EGlobalType>

    type Table =
        private
        | Table of ImmutableSortedSet<Symbol<Table>>

    let empty =
        let cgtypes type1 type2 =
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
        ImmutableSortedSet.Empty
        |> SortedSet.withComparer
            (fun s1 s2 ->
                match (s1, s2) with
                | (NamespaceSymbol(ns1, _), NamespaceSymbol(ns2, _)) ->
                    compare ns1 ns2
                | (NamespaceSymbol _, TypeSymbol _) -> -1
                | (TypeSymbol _, NamespaceSymbol) -> 1
                | (TypeSymbol t1, TypeSymbol t2) ->
                    cgtypes t1 t2)
        |> Table

    /// Retrieves the global types in the specified namespace.
    let nstypes (Namespace ns) =
        let rec inner nsnames (Table table) =
            match nsnames with
            | name :: rest ->
                let equiv = NamespaceSymbol(name, empty)
                match table.TryGetValue equiv with
                | (true, NamespaceSymbol (_, ntable)) ->
                    inner rest ntable
                | _ ->
                    Seq.empty
            | _ ->
                Seq.choose
                    (function
                    | NamespaceSymbol _ -> None
                    | TypeSymbol tdef -> Some tdef)
                    table
        inner ns

    let addSymbol tdef ns globals =
        invalidOp "no"

type GlobalsTable = GlobalsTable.Table
