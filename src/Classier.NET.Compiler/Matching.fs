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

open Classier.NET.Compiler.Item

type MatchResult<'Match, 'Result> =
    | Success of 'Result * Item<'Match>
    | Failure of string * string

type MatchFunc<'Match, 'Result> =
    | Match of string * (Item<'Match> -> MatchResult<'Match, 'Result>)

    member this.Label =
        let (Match (label, _)) = this
        label

/// Determines whether a result is a success.
let isSuccess (result: MatchResult<'Match, 'Result>) =
    match result with
    | Success _ -> true
    | Failure _ -> false

let evaluateMatch (f: MatchFunc<'Match, 'Result>) item =
    let (Match (_, matchFunc)) = f
    matchFunc(item)

let mapResult (resultMap: 'Result1 -> 'Result2) (result: MatchResult<'Match, 'Result1>) =
    match result with
    | Success (success, item) ->
        Success (resultMap success, item);
    | Failure (label, msg) ->
        Failure (label, msg)

let labelResult label (result: MatchResult<'Match, 'Result>) =
    match result with
    | Success _ -> result
    | Failure (_, msg) ->
        Failure (label, msg)

/// Returns a match that is always successful.
let success label (result: 'Result) =
    Match (label, fun (cur: Item<'T>) -> Success (result, cur))

/// Returns a match that is always a failure.
let failure label msg: MatchFunc<'Match, 'Result> =
    Match (label, fun _ -> Failure (label, msg))

/// Changes the label of the specified match.
let labelMatch label (m: MatchFunc<'Match, 'Result>) =
    Match (label, fun item ->
        evaluateMatch m item
        |> labelResult label)

let labelsOfMatches (matches: seq<MatchFunc<'Match, 'Result>>) =
    matches |> Seq.map (fun m -> m.Label)

/// Inserts the specified message into the beginning of the message used when the specified match function fails.
let addFailMsg msg (f: MatchFunc<'Match, 'Result>) =
    Match (f.Label, fun item ->
        let result = evaluateMatch f item
        match result with
        | Success _ -> result
        | Failure (label, oldMsg) ->
            let combinedMsg = sprintf "%s %s" msg oldMsg
            Failure (label, combinedMsg))

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
    f |> mapMatch Seq.singleton

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

let matchAnyOf (items: seq<'T>) (f: 'T -> MatchFunc<'Match, 'Result>) =
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
            evaluateMatch (matchAsSeq f) item
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
let matchChain (matches: seq<MatchFunc<'Match, 'Result>>): MatchFunc<'Match, seq<'Result>> =
    let next (prevResult, item) f: 'Result option * (MatchResult<'Match, 'Result> * Item<'Match> option) =
        match item with
        | Some currentItem ->
            let result = evaluateMatch f currentItem
            match result with
            | Success (success, nextItem) ->
                Some success, (result, Some nextItem)
            | Failure _ ->
                None, (result, None)
        | None ->
            None, (prevResult, None)

    let chainLabel = sprintf "chain [%s]" (matches |> labelsOfMatches |> String.concat ", ")
    Match (chainLabel, fun item ->
        let (matchResults, (lastResult, lastItem)) =
            matches
            |> Seq.mapFold next (Failure (chainLabel, "This should be ignored."), Some item)

        match lastItem with
        | Some lastItem ->
            let results =
                matchResults
                |> Seq.where (fun result -> result.IsSome)
                |> Seq.map (fun result -> result.Value)

            Success (results, lastItem)
        | None ->
            lastResult |> mapResult Seq.singleton)

/// Skips items in the sequence until the specified function returns a success,
/// and returns the skipped items along with the result of the function.
let matchTo (f: MatchFunc<'Match, 'Result>) =
    let toLabel = sprintf "to %s" f.Label
    Match (toLabel, fun startItem ->
        let mapToResult result =
            result
            |> mapResult (fun r -> Seq.empty, r)

        match startItem with
        | Item _ ->
            let (lastItem, lastResult) =
                startItem.AsSequence()
                |> Seq.pick (fun (item, _, _) ->
                    let currentMatch = evaluateMatch f item
                    match currentMatch with
                    | Success (success, nextItem) -> Some (item, Success (success, nextItem))
                    | Failure (_, msg) ->
                        if item.Next.ReachedEnd
                        then Some (item, Failure (toLabel, msg))
                        else None)
            
            match lastResult with
            | Success (success, nextItem) ->
                let matchesBefore =
                    startItem.AsSequence()
                    |> Seq.takeWhile (fun (_, _, index) -> index < lastItem.Index)
                    |> Seq.map (fun (_, m, _) -> m)
                Success ((matchesBefore, success), nextItem)
            | Failure _ ->
                lastResult |> mapToResult
        | End _ ->
            evaluateMatch f startItem
            |> mapToResult
            |> labelResult toLabel)

let matchToEnd: MatchFunc<'Match, seq<'Match>> =
    Match ("end", fun item ->
        match item with
        | Item _ ->
            let (matches, lastItem) =
                item.AsSequence()
                |> Seq.mapFold (fun _ (currentItem, m, _) -> m, currentItem.Next) (End -1)
            Success (matches, lastItem)
        | End _ ->
            Success (Seq.empty, item))

/// Skips items in the sequence until the specified function returns a success, and returns the skipped items.
let matchUntil (f: MatchFunc<'Match, 'Result>) =
    let untilLabel = sprintf "until %s" f.Label
    Match (untilLabel, fun item ->
        match evaluateMatch (matchTo f) item with
        | Success ((matches, _), _) ->
            let lastItem =
                item.AsSequence()
                |> Seq.map (fun (i, _, _) -> i.Next)
                |> Seq.zip matches
                |> Seq.map (fun (_, i) -> i)
                |> Seq.tryLast
                |> Option.orElse (Some item)

            Success (matches, lastItem.Value);
        | Failure (_, msg) ->
            Failure (untilLabel, msg))

/// Matches against the first function, then matches with the specfiied filter function.
/// If the filter function fails or has a success outside of the range of the first result,
/// then the match succeeds.
let matchWithout (filter: MatchFunc<'Match, 'Result>) (f: MatchFunc<'Match, 'Result>) =
    let withoutLabel = sprintf "%s without %s" f.Label filter.Label
    Match (withoutLabel, fun startItem ->
        let firstResult = evaluateMatch f startItem
        match firstResult with
        | Success (_, lastItem) ->
            match evaluateMatch (matchUntil filter) startItem with
            | Success (_, item) ->
                if item.Index <= lastItem.Index then
                    let msg = sprintf "Unexpected %s." filter.Label
                    Failure (withoutLabel, msg)
                else
                    firstResult
            | Failure _ ->
                firstResult
        | Failure (_, msg) ->
            Failure (withoutLabel, msg))

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
        | Item (current: 'Match, _, next) ->
            if predicate current then
                Success (current, next.Value)
            else
                Failure (label, sprintf "Unexpected '%s'." (current.ToString()))
        | End _ -> Failure (label, "The end of the sequence was unexpectedly reached."))
