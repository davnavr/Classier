[<RequireQualifiedAccess>]
module Classier.NET.Compiler.IR.InterfaceSet

open System.Collections.Immutable

open Classier.NET.Compiler
open Classier.NET.Compiler.Extern

let private inamespace =
    function
    | Extern eintf ->
        match eintf with
        | EInterface.Global gintf ->
            Identifier.umap gintf.InterfaceName |> Some
        | EInterface.Nested _ -> None
    | Defined dintf ->
        match dintf with
        | GenInterface.Global gintf ->
            Identifier.umap gintf.InterfaceName |> Some
        | GenInterface.Nested _ -> None

let private iname =
    function
    | Extern (eintf: EInterface) -> Identifier.umap eintf.InterfaceName
    | Defined (dintf: GenInterface) -> Identifier.umap dintf.InterfaceName

let private icompare (i1: ResolvedInterface) (i2: ResolvedInterface) =
    match (inamespace i1, inamespace i2) with
    | (Some ns1, Some ns2) ->
        match compare ns1 ns2 with
        | 0 -> compare (iname i1) (iname i2)
        | ns -> ns
    | (None, Some _) -> -1
    | (Some _, None)
    | (None, None) -> 1

let empty: InterfaceSet =
    SortedSet.withComparer
        icompare
        ImmutableSortedSet.Empty
