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
    | Item of 'T * int * Lazy<Item<'T>>
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

    [<System.Obsolete("Use MatchResult<Match, Result>.Success instead to get the result of a successful match.")>]
    member this.AsSequence() =
        seq {
            let mutable item = this
            while not item.ReachedEnd do
                yield item
                item <- item.Next
        }

type MatchResult<'Match, 'Result> =
    | Success of 'Result * Item<'Match>
    | Failure of string

type MatchFunc<'Match, 'Result> =
    | Match of string * (Item<'Match> -> MatchResult<'Match, 'Result>) // TODO: Add an object that provides error messages

    member this.Label =
        let (Match (label, _)) = this
        label

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

// exclusive
[<System.Obsolete("Use MatchResult<Match, Result>.Success instead to get the result of a successful match.")>]
let rec selectItems (fromItem: Item<'T>) (toItem: Item<'T>): seq<'T> =
    if fromItem.Index > toItem.Index then
        selectItems toItem fromItem
    else
        match fromItem with
        | Item _ ->
            fromItem.AsSequence()
            |> Seq.takeWhile(fun item -> item.Index < toItem.Index)
            |> Seq.map(fun entry ->
                match entry with
                | Item (item, _, _) -> item
                | End index -> invalidOp (sprintf "The entry at index '%i' should not indicate the end of the collection." index))
        | End _ -> Seq.empty

/// Determines whether a result is a success.
let isSuccess (result: MatchResult<'Match, 'Result>) =
    match result with
    | Success _ -> true
    | Failure _ -> false

let result (f: MatchFunc<'Match, 'Result>, item) = // TODO: Return the label as well?
    let (Match (_, matchFunc)) = f
    matchFunc(item)

let labelsOfMatches (matches: seq<MatchFunc<'Match, 'Result>>) =
    matches |> Seq.map (fun m -> m.Label)

/// Returns a match that is always successful.
let success label (result: 'Result) =
    Match (label, fun (cur: Item<'T>) -> Success (result, cur))

/// Returns a match that is always a failure.
let failure label msg: MatchFunc<'Match, 'Result> =
    Match (label, fun _ -> Failure msg)

let labelMatch (m: MatchFunc<'Match, 'Result>) label =
    Match (label, fun item -> result (m, item))

/// Matches with the first function, then feeds the resulting item into the second function.
let andMatch (m1: MatchFunc<'Match, 'Result>) (m2: MatchFunc<'Match, 'Result>): MatchFunc<'Match, seq<'Result>> =
    let andLabel = sprintf "%s and %s" m1.Label m2.Label
    Match (andLabel, fun item1 ->
        let match1 = result (m1, item1)

        match match1 with
        | Failure msg -> Failure msg
        | Success (result1, item2) ->
            let match2 = result (m2, item2)

            match match2 with
            | Failure msg -> Failure msg
            | Success (result2, finalItem) ->
                let finalResult = [result1; result2] |> Seq.ofList
                Success (finalResult, finalItem))

/// Matches with the first function, if the result if a failure, then matches the second function.
let orMatch (m1: MatchFunc<'Match, 'Result>) (m2: MatchFunc<'Match, 'Result>) =
    let orLabel = sprintf "%s or %s" m1.Label m2.Label
    Match (orLabel, fun item ->
        let result1 = result (m1, item)

        match result1 with
        | Failure _ -> result (m2, item)
        | Success _ -> result1)

// Matches with the specified function, then converts the result of the match to another value.
let mapMatch (resultMap: 'Result1 -> 'Result2) (f: MatchFunc<'Match, 'Result1>) =
    let label = sprintf "bind %s" f.Label
    Match (label, fun (item: Item<'Match>) ->
        match result (f, item) with
        | Success (success, nextItem) ->
            Success (resultMap success, nextItem)
        | Failure msg -> Failure msg)

/// Matches against each of the specified functions, and returns the first success found.
let matchAny (matches: seq<MatchFunc<'Match, 'Result>>): MatchFunc<'Match, 'Result> =
    let anyLabel = sprintf "any [%s]" (matches |> labelsOfMatches |> String.concat ", ")
    Match (anyLabel, fun item ->
        let results =
            matches
            |> Seq.map(fun f -> result (f, item))
            |> Seq.cache
        match results |> Seq.tryFind isSuccess with
        | Some result -> result
        | None -> results |> Seq.last)

let matchAnyOf (items: seq<'Match>) (f: 'Match -> MatchFunc<'Match, 'Result>) =
    items |> Seq.map f |> matchAny

/// Matches 1 or more of the specified function.
let matchMany (f: MatchFunc<'Match, 'Result>): MatchFunc<'Match, seq<'Result>> =
    let manyLabel = sprintf "many %s" f.Label

    let nextMatch (item: Item<'Match>) =
        if item.ReachedEnd then
            None
        else
            let currentMatch = result (f, item)
            match currentMatch with
            | Success (_, nextItem) ->
                Some (currentMatch, nextItem)
            | Failure _ -> None

    let nextResult _ (currentResult: MatchResult<'Match, 'Result>) =
        match currentResult with
        | Success (result, nextItem) ->
            (result, nextItem)
        | Failure _ ->
            invalidOp "The match was expected to be a success. The creation of the sequence should stop when the match is a failure."

    Match (manyLabel, fun item ->
        let (matches, lastItem) =
            Seq.unfold nextMatch item
            |> Seq.mapFold nextResult item

        if matches |> Seq.isEmpty then
            let mapToSeq = mapMatch (fun item -> [item] |> Seq.ofList)
            result (mapToSeq f, item)
        else
            Success (matches, lastItem))

/// Matches against the specified function, and returns an empty success if the function fails.
let matchOptional (f: MatchFunc<'Match, 'Result>) =
    let optionalLabel = sprintf "optional %s" f.Label
    Match (optionalLabel, fun item ->
        let r = result (f, item)

        match r with
        | Success (success, nextItem) ->
            Success (Some success, nextItem)
        | Failure _ ->
            Success (None, item))

/// Matches against the first function in the sequence, then feeds the resulting item into the next and so on,
/// until the last function is called or any of the functions fails.
let matchChain (matches: seq<MatchFunc<'Match, 'Result>>) =
    let chainLabel = sprintf "chain [%s]" (matches |> labelsOfMatches |> String.concat ", ")
    Match (chainLabel, fun item ->
        result(matches |> Seq.reduce andMatch, item))

/// Matches against the function until it is a success, exclusive.
let matchUntil (f: MatchFunc<'Match, 'Result>) =
    let untilLabel = sprintf "until %s" f.Label
    Match (untilLabel, fun startItem ->
        match startItem with
        | Item _ ->
            startItem.AsSequence()
            |> Seq.pick (fun item ->
                let currentMatch = result (f, item)
                match currentMatch with
                | Success _ -> Some (Success item) // Returned item is the first item in the success.
                | Failure (msg, _) ->
                    // Check if the last item was reached.
                    if currentMatch.Item.Next.ReachedEnd
                    then Some (Failure (msg, startItem))
                    else None)
        | End _ ->
            result (f, startItem))

/// Matches until the end of the item sequence is reached.
let matchUntilEnd =
    Match ("untilEnd", fun (item: Item<'T>) ->
        if item.ReachedEnd then
            Success item
        else
            Success (item.AsSequence() |> Seq.last).Next)

/// Matches against the function until it is a success, inclusive.
let matchTo f = andMatch (matchUntil f) f

let matchWithout (filter: MatchFunc<'Match, 'Result>) (matchFunc) =
    let withoutLabel = sprintf "%s without %s" matchFunc.Label filter.Label
    Match (withoutLabel, fun item ->
        let initialResult = result (matchFunc, item)
        match initialResult with
        | Success _ ->
            match result (matchUntil filter, item) with
            | Success filterMatch ->
                if filterMatch.Index <= initialResult.Item.Index
                then Failure ("The inverted match failed.", item)
                else initialResult
            | Failure _ -> initialResult
        | Failure _ -> initialResult)

let matchLazy (f: Lazy<MatchFunc<'Match, 'Result>>) =
    Match ("lazy", fun item -> result (f.Value, item))

/// Matches an item and returns the generated result if the predicate is satisfied.
let matchPredicate (predicate: 'Match -> bool) label (result: 'Match -> 'Result) =
    let failWith msg =
        Failure (sprintf "Error matching '%s'. %s" label msg)

    Match (label, fun item ->
        match item with
        | Item (current, _, next) ->
            if predicate current then
                Success (result current, next.Value)
            else
                failWith "The predicate was not satified."
        | End _ -> failWith "The end of the sequence was unexpectedly reached.")
