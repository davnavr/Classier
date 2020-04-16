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

type MatchResult<'T> =
    | Success of seq<'T> * (Item<'T> option)
    | Failure of string * string

type MatchFunc<'T> =
    | Match of string * (Item<'T> -> MatchResult<'T>)

    member this.Label =
        let (Match (label, _)) = this
        label

/// Determines whether a result is a success.
let isSuccess (result: MatchResult<'T>) =
    match result with
    | Success _ -> true
    | Failure _ -> false

/// Returns the result of matching the specified item against the specified matching function.
let evaluateMatch (f: MatchFunc<'T>) item =
    match item with
    | Some actualItem ->
        let (Match (_, matchFunc)) = f
        matchFunc(actualItem)
    | None ->
        Failure (f.Label, "The end of the sequence was unexpectedly reached.")

/// Changes the label of the specified result.
let labelResult label (result: MatchResult<'T>) =
    match result with
    | Success _ -> result
    | Failure (_, msg) ->
        Failure (label, msg)

/// Returns a match that is always successful.
let success label (result: seq<'T>) =
    Match (label, fun (cur: Item<'T>) -> Success (result, Some cur))

/// Returns a match that is always a failure.
let failure label msg: MatchFunc<'T> =
    Match (label, fun _ -> Failure (label, msg))

/// Changes the label of the specified match.
let labelMatch label (m: MatchFunc<'T>) =
    Match (label, fun item ->
        evaluateMatch m (Some item)
        |> labelResult label)

/// Returns a sequence containing the labels of the specified matching functions.
let labelsOfMatches (matches: seq<MatchFunc<'T>>) =
    matches |> Seq.map (fun m -> m.Label)

/// Matches with the first function, then feeds the resulting item into the second function.
let andMatch (m1: MatchFunc<'T>) (m2: MatchFunc<'T>) =
    let andLabel = sprintf "%s and %s" m1.Label m2.Label
    Match (andLabel, fun item1 ->
        let match1 = evaluateMatch m1 (Some item1)

        match match1 with
        | Failure (_, msg) -> Failure (andLabel, msg)
        | Success (result1, item2) ->
            let match2 = evaluateMatch m2 item2

            match match2 with
            | Failure (_, msg) -> Failure (andLabel, msg)
            | Success (result2, finalItem) ->
                let finalResult = Seq.append result1 result2
                Success (finalResult, finalItem))

/// Matches with the first function, if the result if a failure, then matches the second function.
let orMatch (m1: MatchFunc<'T>) (m2: MatchFunc<'T>) =
    let orLabel = sprintf "%s or %s" m1.Label m2.Label
    Match (orLabel, fun item ->
        let result1 = evaluateMatch m1 (Some item)

        match result1 with
        | Failure _ -> evaluateMatch m2 (Some item)
        | Success _ -> result1)

/// Matches against each of the specified functions, and returns the first success found.
let matchAny (matches: seq<MatchFunc<'T>>) =
    let anyLabel = sprintf "any [%s]" (matches |> labelsOfMatches |> String.concat ", ")
    Match (anyLabel, fun item ->
        let results =
            matches
            |> Seq.map(fun f -> evaluateMatch f (Some item))
            |> Seq.cache
        match results |> Seq.tryFind isSuccess with
        | Some result -> result
        | None -> results |> Seq.last)

let matchAnyOf items (f: 'Match -> MatchFunc<'T>) =
    items |> Seq.map f |> matchAny

/// Matches one or more of the specified function.
let matchMany (f: MatchFunc<'T>): MatchFunc<'T> =
    let manyLabel = sprintf "many %s" f.Label

    Match (manyLabel, fun item ->
        Failure (manyLabel, "Not implemented."))

/// Matches against the specified function, and returns an empty success if the function fails.
let matchOptional (f: MatchFunc<'T>) =
    let optionalLabel = sprintf "optional %s" f.Label
    Match (optionalLabel, fun item ->
        let result = evaluateMatch f (Some item)

        match result with
        | Success _ -> result
        | Failure _ -> Success (Seq.empty, Some item))

/// Matches against the first function in the sequence, then feeds the resulting item into the next and so on,
/// until the last function is called or any of the functions fails.
let matchChain (matches: seq<MatchFunc<'T>>) =
    let chainLabel = sprintf "chain [%s]" (matches |> labelsOfMatches |> String.concat ", ")
    if matches |> Seq.isEmpty then
        success chainLabel Seq.empty
    else
        matches
        |> Seq.reduce andMatch
        |> labelMatch chainLabel

/// Skips items in the sequence until the specified function returns a success,
/// and returns the skipped items along with the result of the function.
let matchTo (f: MatchFunc<'T>): MatchFunc<'T> =
    let toLabel = sprintf "to %s" f.Label
    Match (toLabel, fun startItem ->
        Failure (toLabel, "NOT IMPLEMENTED"))

let matchToEnd: MatchFunc<'T> =
    Match ("end", fun item ->
        Failure ("end", "NOT IMPLEMENTED"))

/// Skips items in the sequence until the specified function returns a success, and returns the skipped items.
let matchUntil (f: MatchFunc<'T>): MatchFunc<'T> =
    let untilLabel = sprintf "until %s" f.Label
    Match (untilLabel, fun item ->
        Failure (untilLabel, "NOT IMPLEMENTED"))

/// Matches against the first function, then matches with the specfiied filter function.
/// If the filter function fails or has a success outside of the range of the first result,
/// then the match succeeds.
let matchWithout (filter: MatchFunc<'T>) (f: MatchFunc<'T>): MatchFunc<'T> =
    let withoutLabel = sprintf "%s without %s" f.Label filter.Label
    Match (withoutLabel, fun startItem ->
        Failure (withoutLabel, "NOT IMPLEMENTED"))

/// Lazily evaluates the given matching function.
let matchLazy (f: Lazy<MatchFunc<'T>>) =
    let lazyLabel state = sprintf "lazy (%s)" state
    Match (lazyLabel "unevaluated", fun item ->
        let result = evaluateMatch f.Value (Some item)
        match result with
        | Failure (label, msg) ->
            Failure (lazyLabel label, msg)
        | Success _ -> result)

/// Matches an item and returns the generated result if the predicate is satisfied.
let matchPredicate label (predicate: 'T -> bool) =
    Match (label, fun item ->
        if predicate item.Value then
            Success (item.Value |> Seq.singleton, item.Next.Value)
        else
            Failure (label, sprintf "Unexpected %A." item.Value))
