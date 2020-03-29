// Copyright (c) 2020, David Navarro. All rights reserved.
// Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.

module Classier.NET.Compiler.Matching

type Item<'T> =
    | Item of Item : 'T * Index : int * Next : Lazy<Item<'T>>
    /// Indicates the end of a sequence.
    | End

type MatchResult<'T> =
    | Success of Item<'T>
    | Failure of string * Item<'T>

/// <summary>
/// Represents a function used to match against something.
/// </summary>
/// <typeparam name="T">The type of the object to match.</typeparam>
type MatchFunc<'T> = Match of (Item<'T> -> MatchResult<'T>)

/// <summary>
/// Creates an item containing the first element of the specified sequence.
/// </summary>
/// <param name="items">The sequence contains the items.</param>
/// <returns>An <see cref="Item{T}.Item"/> containing the first element of the sequenc; or <see cref="Item{T}.End"/> if the sequence has no items.</returns>
let itemFrom<'T> (items: seq<'T>): Item<'T> =
    let enumerator = items.GetEnumerator()
    let rec nextItem index =
        if enumerator.MoveNext() then
            Item(enumerator.Current, index, lazy nextItem (index + 1))
        else
            enumerator.Dispose() |> ignore
            End
    nextItem 0

let itemIsEnd<'T> (item: Item<'T>) =
    match item with
    | End -> true
    | Item  _ -> false

/// <summary>
/// Gets the index of the specified item.
/// </summary>
/// <param name="item"></param>
/// <returns>The index of the specified item; or <c>-1</c> if the item indicated the end of the sequence.</returns>
let itemIndex<'T> (item: Item<'T>) =
    match item with
    | Item (_, i, _) -> i
    | End -> -1

let nextItem<'T> (item: Item<'T>) =
    match item with
    | Item (_, _, next) -> next.Value
    | End -> End

/// <summary>
/// Determines whether a result is a success.
/// </summary>
/// <param name="result">The result to check.</param>
/// <returns><see langword="true"/> if the <paramref name="result"/> is a success; otherwise <see langword="false"/>.</result>
let isSuccess result =
    match result with
    | Success _ -> true
    | Failure _ -> false

let asSuccess result =
    match result with
    | Success item -> item
    | Failure _ -> invalidArg "result" "The result must indicate a success."

let asFailure result =
    match result with
    | Success _ -> invalidArg "result" "The result must indicate a failure."
    | Failure (msg, item) -> (msg, item)

let result<'T> (f: MatchFunc<'T>, item) =
    let (Match matchFunc) = f
    matchFunc(item)

let success<'T> =
    Match (fun (cur: Item<'T>) -> Success cur)

let failure<'T> msg =
    Match (fun (cur: Item<'T>) -> Failure(msg, cur))

// and
let thenMatch<'T> (m1: MatchFunc<'T>) m2 =
    Match (fun item1 ->
        let result1 = result (m1, item1)
        match result1 with
        | Failure _ -> result1
        | Success item2 -> result (m2, item2))

let orMatch<'T> (m1: MatchFunc<'T>) m2 =
    Match (fun item ->
        let result1 = result (m1, item)
        match result1 with
        | Failure _ -> result (m2, item)
        | Success _ -> result1)

let matchAny<'T> (matches: seq<MatchFunc<'T>>) =
    if matches |> Seq.isEmpty then
        failure "Cannot match any, the match sequence was empty."
    else
        Match (fun item ->
            let results =
                matches
                |> Seq.map(fun f -> result (f, item))
                |> Seq.cache
            match results |> Seq.tryFind isSuccess with
            | Some result -> result
            | None -> results |> Seq.last)

/// <summary>
/// Matches against the specified character.
/// </summary>
/// <param name="char">The character to match.</param>
let matchChar char: MatchFunc<char> =
    let failMsg r = sprintf "Expected character %c, but %s" char r
    Match (fun cur ->
        match cur with
        | Item (act, _, next) ->
            if char = act then
                Success next.Value
            else
                Failure (failMsg (sprintf "got %c instead" act), cur)
        | End -> Failure (failMsg "the end of the text was reached instead.", cur))

/// <summary>
/// Matches against a sequence of characters.
/// </summary>
/// <param name="str">The expected sequence of characters.</param>
let matchStr str: MatchFunc<char> =
    if str |> Seq.isEmpty then
        success
    else
        Match (fun item ->
            let r = result (str |> Seq.map matchChar |> Seq.reduce thenMatch, item)
            match r with
            | Success _ -> r
            | Failure (msg, _) -> Failure (sprintf "Failure parsing '%s'. %s" str msg, item))

let matchAnyStr (strings: seq<string>) =
    match strings with
    | empty when empty |> Seq.isEmpty ->
        success
    | _ ->
        strings |> Seq.map matchStr |> matchAny
