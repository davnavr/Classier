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

/// Provides helper functions for interacting with sorted sets.
[<RequireQualifiedAccess>]
module SortedSet =
    open System.Collections.Generic

    let (|Contains|_|) item (set: ImmutableSortedSet<_>) =
        set.Contains(item) |> Bool.toOpt

    let (|Empty|_|) (set: ImmutableSortedSet<_>) =
        Bool.toOpt set.IsEmpty

    let add item (set: ImmutableSortedSet<_>) = set.Add item

    let tryAdd item (set: ImmutableSortedSet<_>) =
        match set with
        | Contains item -> None
        | _ -> set |> add item |> Some

    let remove item (set: ImmutableSortedSet<_>) = set.Remove item

    let withComparison comparer (set: ImmutableSortedSet<_>) =
        { new IComparer<_> with
              member _.Compare(one, two) =
                  comparer one two }
        |> set.WithComparer

[<RequireQualifiedAccess>]
module ImmList =
    let (|Contains|_|) item (list: ImmutableList<_>) =
        list.Contains item |> Bool.toOpt

    let map mapping (list: ImmutableList<_>) =
        new System.Func<_, _>(mapping) |> list.ConvertAll

[<RequireQualifiedAccess>]
module Regex =
    open System.Text.RegularExpressions

    let (|Matches|_|) (reg: Regex) str =
        reg.IsMatch str |> Bool.toOpt

[<AutoOpen>]
module ResultBuilder =
    type ResultBuilder() =
        member _.Bind(r, binder) =
            Result.bind binder r
        member _.Return(r) = Result.Ok r
        member _.ReturnFrom(r: Result<_, _>) = r

    let result = ResultBuilder()
