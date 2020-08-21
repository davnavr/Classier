[<RequireQualifiedAccess>]
module Classier.NET.Compiler.IR.InterfaceSet

open System.Collections.Immutable

open Classier.NET.Compiler

type private InterfaceParent =
    | ParentNamespace of Namespace
    | ParentGen of GenInterface
    | ParentRef of RefInterface

let private info =
    function
    | Gen (GenInterface.Global { InterfaceName = Identifier.Simplify iname; Parent = ns })
    | Ref (RefInterface.Global { InterfaceName = Identifier.Simplify iname; Parent = ns }) ->
        iname, ParentNamespace ns
    | Gen (GenInterface.Nested { InterfaceName = Identifier.Simplify iname; Parent = parent }) ->
        iname, ParentGen parent
    | Ref (RefInterface.Nested { InterfaceName = Identifier.Simplify iname; Parent = parent })->
        iname, ParentRef parent

let private icompare i1 i2 =
    let (name1, parent1) = info i1
    let (name2, parent2) = info i2
    match (compare name1 name2, parent1, parent2) with
    | (0, ParentNamespace _, ParentGen _)
    | (0, ParentNamespace _, ParentRef _)
    | (0, ParentGen _, ParentRef _) -> 1
    | (0, ParentGen _, ParentNamespace _)
    | (0, ParentRef _, ParentNamespace)
    | (0, ParentRef _, ParentGen _) -> -1
    | (0, ParentNamespace ns1, ParentNamespace ns2) ->
        invalidOp "bad"
    | (0, ParentGen parent1, ParentGen parent2) ->
        invalidOp "bad"

    //match (i1, i2) with
    //| (Info (_, ParentNamespace _), Info (_, ParentGen))
    //| (Info (_, ParentNamespace _), Info (_, ParentRef))
    //| (Info (_, ParentGen _), Info (_, ParentRef _)) -> 1
    //| (Info (_, ParentGen), Info (_, ParentNamespace _))
    //| (Info (_, ParentRef), Info (_, ParentNamespace _)) -> -1
    //| (Info (name1, ParentNamespace ns1), Info (name2, ParentNamespace ns2)) ->
    //    invalidOp "bad"

    //match (i1, i2) with
    //| (GenInterface.Global gi1, GenInterface.Global gi2) ->
    //    match compare gi1.Parent gi2.Parent with
    //    | 0 -> Identifier.ncompare gi1.InterfaceName gi2.InterfaceName
    //    | ns -> ns
    //| (GenInterface.Nested _, GenInterface.Global _) -> -1
    //| (GenInterface.Global _, GenInterface.Nested)
    //| (GenInterface.Nested _, GenInterface.Nested _) -> 1

let empty: GenInterfaceSet =
    SortedSet.withComparer
        icompare
        ImmutableSortedSet.Empty
