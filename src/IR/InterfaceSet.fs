[<RequireQualifiedAccess>]
module Classier.NET.Compiler.IR.InterfaceSet

open System.Collections.Immutable

open Classier.NET.Compiler

let private icompare i1 i2 =
    match (i1, i2) with
    | (GenInterface.Global gi1, GenInterface.Global gi2) ->
        match compare gi1.Parent gi2.Parent with
        | 0 -> Identifier.ncompare gi1.InterfaceName gi2.InterfaceName
        | ns -> ns
    | (GenInterface.Nested _, GenInterface.Global _) -> -1
    | (GenInterface.Global _, GenInterface.Nested)
    | (GenInterface.Nested _, GenInterface.Nested _) -> 1

let empty: GenInterfaceSet =
    SortedSet.withComparer
        icompare
        ImmutableSortedSet.Empty
