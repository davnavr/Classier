[<RequireQualifiedAccess>]
module Classier.NET.Compiler.IR.InterfaceSet

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Extern

let empty: InterfaceSet =
    let icompare ((ns1, i1): ResolvedInterface) ((ns2, i2): ResolvedInterface) =
        match compare ns1 ns2 with
        | 0 ->
            match (i1, i2) with
            | (Extern e1, Extern e2) ->
                compare
                    (Identifier.noGenerics e1.InterfaceName)
                    (Identifier.noGenerics e2.InterfaceName)
            | (Defined d1, Defined d2) ->
                compare
                    (Identifier.noGenerics d1.InterfaceName)
                    (Identifier.noGenerics d2.InterfaceName)
        | ns -> ns
    SortedSet.withComparison
        icompare
        ImmutableSortedSet.Empty
