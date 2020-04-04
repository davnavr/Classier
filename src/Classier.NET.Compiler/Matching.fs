// Copyright (c) 2020 David Navarro
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

module Classier.NET.Compiler.Matching

type Item<'T> =
    | Item of Item : 'T * Index : int * Next : Lazy<Item<'T>>
    /// Indicates the end of a sequence.
    | End

    /// Gets the index of the specified item.
    member this.Index =
        match this with
        | Item (_, index, _) -> index
        | End -> -1

    /// Gets the next item.
    member this.Next =
        match this with
        | Item (_, _, next) -> next.Value
        | End -> this

    /// Gets a value indicating whether the next item in the sequence is available.
    member this.HasNext =
        match this.Next with
        | Item _ -> true
        | End -> false

type MatchResult<'T> =
    | Success of Item<'T> // TODO: Include a seq<'T> in the Success?
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
let itemFrom (items: seq<'T>): Item<'T> =
    let enumerator = items.GetEnumerator()
    let rec nextItem index =
        if enumerator.MoveNext() then
            Item(enumerator.Current, index, lazy nextItem (index + 1))
        else
            enumerator.Dispose() |> ignore
            End
    nextItem 0

/// <summary>
/// Determines whether a result is a success.
/// </summary>
/// <param name="result">The result to check.</param>
/// <returns><see langword="true"/> if the <paramref name="result"/> is a success; otherwise <see langword="false"/>.</result>
let isSuccess result =
    match result with
    | Success _ -> true
    | Failure _ -> false

[<System.Obsolete>]
let asSuccess result =
    match result with
    | Success item -> item
    | Failure _ -> invalidArg "result" "The result must indicate a success."

[<System.Obsolete>]
let asFailure result =
    match result with
    | Success _ -> invalidArg "result" "The result must indicate a failure."
    | Failure (msg, item) -> (msg, item)

let result (f: MatchFunc<'T>, item) =
    let (Match matchFunc) = f
    matchFunc(item)

let success =
    Match (fun (cur: Item<'T>) -> Success cur)

let failure msg =
    Match (fun (cur: Item<'T>) -> Failure(msg, cur))

// and
let thenMatch (m1: MatchFunc<'T>) m2 =
    Match (fun item1 ->
        let result1 = result (m1, item1)
        match result1 with
        | Failure _ -> result1
        | Success item2 -> result (m2, item2))

let orMatch (m1: MatchFunc<'T>) m2 =
    Match (fun item ->
        let result1 = result (m1, item)
        match result1 with
        | Failure _ -> result (m2, item)
        | Success _ -> result1)

let matchAny (matches: seq<MatchFunc<'T>>) =
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

let matchAnyOf (items: seq<'TItem>) (f: 'TItem -> MatchFunc<'T>) =
    if items |> Seq.isEmpty then
        success
    else
        items |> Seq.map f |> matchAny

// 1 or more
let matchMany (f: MatchFunc<'T>) =
    Match (fun item ->
        let results =
            (fun _ -> result (f, item))
            |> Seq.initInfinite
            |> Seq.takeWhile isSuccess
    
        if results |> Seq.isEmpty then
            result (f, item)
        else
            results |> Seq.last)

/// <summary>
/// Matches against the specified character.
/// </summary>
/// <param name="char">The character to match.</param>
let matchChar char =
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
let matchStr str =
    if str |> Seq.isEmpty then
        success
    else
        Match (fun item ->
            let r = result (str |> Seq.map matchChar |> Seq.reduce thenMatch, item)
            match r with
            | Success _ -> r
            | Failure (msg, _) -> Failure (sprintf "Failure parsing '%s'. %s" str msg, item))
