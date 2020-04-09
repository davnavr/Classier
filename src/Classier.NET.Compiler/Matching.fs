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

    /// Gets the next item after the current item, or the current item if it indicates the end of the sequence..
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
    | Failure of string * string

type MatchFunc<'Match, 'Result> =
    | Match of string * (Item<'Match> -> MatchResult<'Match, 'Result>)

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

let evaluateMatch (f: MatchFunc<'Match, 'Result>) item = // TODO: Return the label as well?
    let (Match (_, matchFunc)) = f
    matchFunc(item)

let labelsOfMatches (matches: seq<MatchFunc<'Match, 'Result>>) =
    matches |> Seq.map (fun m -> m.Label)

/// Returns a match that is always successful.
let success label (result: 'Result) =
    Match (label, fun (cur: Item<'T>) -> Success (result, cur))

/// Returns a match that is always a failure.
let failure label msg: MatchFunc<'Match, 'Result> =
    Match (label, fun _ -> Failure (label, msg))

let labelMatch label (m: MatchFunc<'Match, 'Result>) =
    Match (label, fun item -> evaluateMatch m item)

/// Matches with the first function, then feeds the resulting item into the second function.
let andMatch (m1: MatchFunc<'Match, 'Result>) (m2: MatchFunc<'Match, 'Result>) =
    let andLabel = sprintf "%s and %s" m1.Label m2.Label
    Match (andLabel, fun item1 ->
        let match1 = evaluateMatch m1 item1

        match match1 with
        | Failure (_, msg) -> Failure (andLabel, msg)
        | Success (result1, item2) ->
            let match2 = evaluateMatch m2 item2

            match match2 with
            | Failure (_, msg) -> Failure (andLabel, msg)
            | Success (result2, finalItem) ->
                let finalResult = (result1, result2)
                Success (finalResult, finalItem))

/// Matches with the first function, if the result if a failure, then matches the second function.
let orMatch (m1: MatchFunc<'Match, 'Result>) (m2: MatchFunc<'Match, 'Result>) =
    let orLabel = sprintf "%s or %s" m1.Label m2.Label
    Match (orLabel, fun item ->
        let result1 = evaluateMatch m1 item

        match result1 with
        | Failure _ -> evaluateMatch m2 item
        | Success _ -> result1)

// Matches with the specified function, then converts the result of the match to another value.
let mapMatch (resultMap: 'Result1 -> 'Result2) (f: MatchFunc<'Match, 'Result1>) =
    let label = sprintf "map %s" f.Label
    Match (label, fun (item: Item<'Match>) ->
        match evaluateMatch f item with
        | Success (success, nextItem) ->
            Success (resultMap success, nextItem)
        | Failure (_, msg) -> Failure (label, msg))

let matchAsSeq (f: MatchFunc<'Match, 'Result>) =
    f |> mapMatch (fun r -> seq { yield r })

/// Matches against each of the specified functions, and returns the first success found.
let matchAny (matches: seq<MatchFunc<'Match, 'Result>>): MatchFunc<'Match, 'Result> =
    let anyLabel = sprintf "any [%s]" (matches |> labelsOfMatches |> String.concat ", ")
    Match (anyLabel, fun item ->
        let results =
            matches
            |> Seq.map(fun f -> evaluateMatch f item)
            |> Seq.cache
        match results |> Seq.tryFind isSuccess with
        | Some result -> result
        | None -> results |> Seq.last)

let matchAnyOf (items: seq<'Match>) (f: 'Match -> MatchFunc<'Match, 'Result>) =
    items |> Seq.map f |> matchAny

/// Matches one or more of the specified function.
let matchMany (f: MatchFunc<'Match, 'Result>): MatchFunc<'Match, seq<'Result>> =
    let manyLabel = sprintf "many %s" f.Label

    let nextMatch (item: Item<'Match>) =
        if item.ReachedEnd then
            None
        else
            let currentMatch = evaluateMatch f item
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
            evaluateMatch (mapToSeq f) item
        else
            Success (matches, lastItem))

/// Matches against the specified function, and returns an empty success if the function fails.
let matchOptional (f: MatchFunc<'Match, 'Result>) =
    let optionalLabel = sprintf "optional %s" f.Label
    Match (optionalLabel, fun item ->
        let result = evaluateMatch f item

        match result with
        | Success (success, nextItem) ->
            Success (Some success, nextItem)
        | Failure _ ->
            Success (None, item))

/// Matches against the first function in the sequence, then feeds the resulting item into the next and so on,
/// until the last function is called or any of the functions fails.
let matchChain (matches: seq<MatchFunc<'Match, 'Result>>) =
    let nextResult (prevItem: Item<'Match> option) (f: MatchFunc<'Match, 'Result>) =
        match prevItem with
        | Some item ->
            let currentResult = evaluateMatch f item
            match currentResult with
            | Success (success, nextItem) ->
                Some success, Some nextItem
            | Failure _ -> None, None
        | None -> None, None

    let chainLabel = sprintf "chain [%s]" (matches |> labelsOfMatches |> String.concat ", ")
    Match (chainLabel, fun item ->
        let (results, lastItem) =
            matches |> Seq.mapFold nextResult (Some item)
        
        match lastItem with
        | Some finalItem -> // TODO: Clean up this mess right here.
            Success (results |> Seq.filter (fun r -> r.IsSome) |> Seq.map (fun r -> r.Value), finalItem)
        | None ->
            evaluateMatch (matches |> Seq.head |> matchAsSeq) item)

/// Lazily evaluates the given matching function.
let matchLazy (f: Lazy<MatchFunc<'Match, 'Result>>) =
    let lazyLabel state = sprintf "lazy (%s)" state
    Match (lazyLabel "unevaluated", fun item ->
        let result = evaluateMatch f.Value item
        match result with
        | Failure (label, msg) ->
            Failure (lazyLabel label, msg)
        | Success _ -> result)

/// Matches an item and returns the generated result if the predicate is satisfied.
let matchPredicate (predicate: 'Match -> bool) label =
    Match (label, fun item ->
        match item with
        | Item (current, _, next) ->
            if predicate current then
                Success (current, next.Value)
            else
                Failure (label, "The predicate was not satified.")
        | End _ -> Failure (label, "The end of the sequence was unexpectedly reached."))
