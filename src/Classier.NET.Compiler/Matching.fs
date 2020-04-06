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
    | End of int

    /// Gets the index of the specified item.
    member this.Index =
        match this with
        | Item (_, index, _) -> index
        | End index -> index

    /// Gets the next item.
    member this.Next =
        match this with
        | Item (_, _, next) -> next.Value
        | End _ -> this
        
    /// Gets a value indicating whether the current item indicates the end of the sequence.
    member this.ReachedEnd =
        match this with
        | Item _ -> false
        | End _ -> true

    // exclusive
    member this.SelectItems (toItem: Item<'T>): seq<'T> =
        if this.Index > toItem.Index then
            toItem.SelectItems this
        else
            match this with
            | Item _ ->
                seq {
                    let mutable item = this
                    while not item.ReachedEnd do
                        yield item
                        item <- item.Next
                }
                |> Seq.takeWhile(fun item -> item.Index < toItem.Index)
                |> Seq.map(fun entry ->
                    match entry with
                    | Item (item, _, _) -> item
                    | End index -> invalidOp (sprintf "The entry at index '%i' should not indicate the end of the collection." index))
            | End _ -> Seq.empty

type MatchResult<'T> =
    | Success of Item<'T> // TODO: Include a seq<'T> in the Success?
    | Failure of string * Item<'T>

    member this.Item =
        match this with
        | Success item -> item
        | Failure (_, item) -> item

/// <summary>
/// Represents a function used to match against something.
/// </summary>
/// <typeparam name="T">The type of the object to match.</typeparam>
type MatchFunc<'T> = Match of (Item<'T> -> MatchResult<'T>)

/// <summary>
/// Creates an item containing the first element of the specified sequence.
/// </summary>
/// <param name="items">The sequence contains the items.</param>
/// <returns>An <see cref="Item{T}.Item"/> containing the first element of the sequence; or <see cref="Item{T}.End"/> if the sequence has no items.</returns>
let itemFrom (items: seq<'T>): Item<'T> =
    let enumerator = items.GetEnumerator()
    let rec nextItem index =
        if enumerator.MoveNext() then
            Item(enumerator.Current, index, lazy nextItem (index + 1))
        else
            enumerator.Dispose() |> ignore
            End index
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

let result (f: MatchFunc<'T>, item) =
    let (Match matchFunc) = f
    matchFunc(item)

// fsharplint:disable-next-line ReimplementsFunction
let success = Match (fun (cur: Item<'T>) -> Success cur)

let failure msg = Match (fun (cur: Item<'T>) -> Failure(msg, cur))

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
        success
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
            seq {
                let mutable prevMatch = result(f, item)
                while isSuccess prevMatch do
                    yield prevMatch
                    prevMatch <- result(f, prevMatch.Item);
            }
            |> Seq.cache

        if results |> Seq.isEmpty then
            result (f, item)
        else
            results |> Seq.last)

let matchOptional (f: MatchFunc<'T>) =
    Match (fun item ->
        let r = result (f, item)
        match r with
        | Success _ -> r
        | Failure _ -> Success item)

let matchChain (items: seq<MatchFunc<'T>>) =
    if items |> Seq.isEmpty then
        success
    else
        //Match (fun startItem ->
        //    let nextResult prevResult f =
        //        match prevResult with
        //        | Failure _ -> prevResult
        //        | Success nextItem -> result (f, nextItem)
        //    Seq.fold nextResult (Success startItem) items)
        Match (fun item -> result(items |> Seq.reduce thenMatch, item))

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
        | End _ -> Failure (failMsg "the end of the text was reached instead.", cur))

/// <summary>
/// Matches against a sequence of characters.
/// </summary>
/// <param name="str">The expected sequence of characters.</param>
let matchStr str =
    if str |> Seq.isEmpty then
        success
    else
        Match (fun item ->
            let r = result (str |> Seq.map matchChar |> matchChain, item)
            match r with
            | Success _ -> r
            | Failure (msg, _) -> Failure (sprintf "Failure parsing '%s'. %s" str msg, item))
