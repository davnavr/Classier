namespace Classier.NET.Compiler

open System.Collections.Immutable

[<RequireQualifiedAccess>]
type TypeOrMember<'Type, 'Member> =
    | Type of 'Type
    | Member of 'Member

[<NoComparison; StructuralEquality>]
type DefinedOrExtern<'Defined, 'Extern> =
    | Defined of 'Defined
    | Extern of 'Extern

[<RequireQualifiedAccess>]
module DefinedOrExtern =
    let map def ext =
        function
        | Defined d -> def d |> Defined
        | Extern e -> ext e |> Extern

module Bool =
    let toOpt b =
        if b
        then Some()
        else None

[<RequireQualifiedAccess>]
module Comparer =
    open System.Collections.Generic

    let create f =
        { new IComparer<_> with
              member _.Compare(one, two) =
                  f one two }

/// Provides helper functions for interacting with sorted sets.
[<RequireQualifiedAccess>]
module SortedSet =
    let (|Contains|_|) item (set: ImmutableSortedSet<_>) =
        set.Contains(item) |> Bool.toOpt

    let (|Empty|_|) (set: ImmutableSortedSet<_>) =
        Bool.toOpt set.IsEmpty

    let inline add item (set: ImmutableSortedSet<_>) =
        set.Add item
    let inline remove item (set: ImmutableSortedSet<_>) =
        set.Remove item
    let inline singleton item = ImmutableSortedSet.Create(item = item)
    let inline withComparer comparer (set: ImmutableSortedSet<_>) =
        Comparer.create comparer |> set.WithComparer

    let tryGetValue equal found missing (set: ImmutableSortedSet<_>) =
        match set.TryGetValue equal with
        | (true, original) -> found original
        | (false, _) -> missing()

    let tryAdd item set =
        tryGetValue
            item
            Result.Error
            (fun() ->
                set
                |> add item
                |> Result.Ok)
            set

[<RequireQualifiedAccess>]
module ImmArray =
    let inline ofArray source = ImmutableArray.Create(items = source)
    let inline singleton item = ImmutableArray.Create(item = item)

[<RequireQualifiedAccess>]
module ImmList =
    let (|Contains|_|) item (list: ImmutableList<_>) =
        list.Contains item |> Bool.toOpt

    let (|Empty|_|) (list: ImmutableList<_>) =
        Bool.toOpt list.IsEmpty

    let map mapping (list: ImmutableList<_>) =
        new System.Func<_, _>(mapping) |> list.ConvertAll

    let inline add item (list: ImmutableList<_>) = list.Add item
    let inline addRange items (list: ImmutableList<_>) = list.AddRange items
    let inline ofSeq (value: seq<_>) = ImmutableList.CreateRange value
    let inline singleton item = ImmutableList.Create(item = item)

    let setItem index item (list: ImmutableList<_>) = list.SetItem(index, item)

[<RequireQualifiedAccess>]
module ImmSortedDict =
    let inline add key value (dict: ImmutableSortedDictionary<_, _>) =
        dict.Add(key, value)
    let inline setItem key value (dict: ImmutableSortedDictionary<_, _>) =
        dict.SetItem(key, value)
    let inline withKeyComparer kcomparer (dict: ImmutableSortedDictionary<_, _>) =
        kcomparer
        |> Comparer.create
        |> dict.WithComparers

    let tryGetValue key found missing (dict: ImmutableSortedDictionary<_, _>) =
        match dict.TryGetValue key with
        | (true, value) -> found value
        | (false, _) -> missing()

    let tryAdd key value (dict: ImmutableSortedDictionary<_, _>) =
        tryGetValue
            key
            Result.Error
            (fun() -> dict.Add(key, value) |> Result.Ok)
            dict

[<RequireQualifiedAccess>]
module Regex =
    open System.Text.RegularExpressions

    let (|Matches|_|) (reg: Regex) str =
        reg.IsMatch str |> Bool.toOpt

[<RequireQualifiedAccess>]
module Result =
    let get r =
        match r with
        | Result.Ok value -> value
        | Result.Error err ->
            err.ToString() |> invalidArg "r"

[<AutoOpen>]
module ResultBuilder =
    type ResultBuilder() =
        member _.Bind(r, binder) =
            Result.bind binder r
        member _.Return(r) = Result.Ok r
        member _.ReturnFrom(r: Result<_, _>) = r

    let result = ResultBuilder()
