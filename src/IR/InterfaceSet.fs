[<RequireQualifiedAccess>]
module Classier.NET.Compiler.IR.InterfaceSet

open System.Collections.Immutable
open Classier.NET.Compiler

let inline private cname intf1 intf2 =
    let inline iname intf =
        ((^a) : (member InterfaceName : _) (intf))
        |> Identifier.noGenerics
    compare (iname intf1) (iname intf2)

let empty: InterfaceSet =
    let icompare ((ns1, i1): ResolvedInterface) ((ns2, i2): ResolvedInterface) =
        match compare ns1 ns2 with
        | 0 ->
            match (i1, i2) with
            | (Extern e1, Extern e2) -> cname e1 e2
            | (Extern ext, Defined def)
            | (Defined def, Extern ext) ->
                match i1 with
                | Extern -> 1
                | Defined -> -1
                |> (*) (cname def ext)
            | (Defined d1, Defined d2) -> cname d1 d2
        | ns -> ns
    SortedSet.withComparer
        icompare
        ImmutableSortedSet.Empty
