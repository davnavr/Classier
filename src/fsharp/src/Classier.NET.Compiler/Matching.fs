// Copyright (c) 2020, David Navarro. All rights reserved.
// Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.

module Classier.NET.Compiler.Matching

open Classier.NET.Compiler.Collections

type MatchResult<'T> =
    | Success of Cursor<'T>
    | Failure of string * Cursor<'T>

/// <summary>
/// Represents a function used to match against something.
/// </summary>
/// <typeparam name="T">The type of the object to match.</typeparam>
type MatchFunc<'T> = Match of (Cursor<'T> -> MatchResult<'T>)

/// <summary>
/// Determines whether a <see cref="Matching.MatchResult<T>"/> is a <see cref="Matching.MatchResult<T>.Success"/>.
/// </summary>
/// <param name="result">The result to check.</param>
/// <returns><see langword="true"/> if the <paramref name="result"/> is a <see cref="Matching.MatchResult<T>.Success"/>; otherwise <see langword="false"/>.</result>
let isSuccess result =
    match result with
    | Success _ -> true
    | Failure _ -> false

let asSuccess result =
    match result with
    | Success cur -> cur
    | Failure _ -> invalidArg "result" "The result must indicate a success."

let asFailure result =
    match result with
    | Success _ -> invalidArg "result" "The result must indicate a failure."
    | Failure (msg, cur) -> (msg, cur)

let result<'T> (f: MatchFunc<'T>, cur) =
    let (Match matchFunc) = f
    matchFunc(cur)

let success<'T> =
    Match (fun (cur: Cursor<'T>) -> Success cur)

let failure<'T> msg =
    Match (fun (cur: Cursor<'T>) -> Failure(msg, cur))

// and
let thenMatch<'T> (m1: MatchFunc<'T>) m2 =
    Match (fun c1 ->
        let r1 = result (m1, c1)
        match r1 with
        | Failure _ -> r1
        | Success c2 -> result (m2, c2))

let orMatch<'T> (m1: MatchFunc<'T>) m2 =
    Match (fun cur ->
        let r1 = result (m1, cur)
        match r1 with
        | Failure _ -> result (m2, cur)
        | Success _ -> r1)

let matchAny<'T> (matches: seq<MatchFunc<'T>>) =
    if matches |> Seq.isEmpty then
        failure "Cannot match any, the match sequence was empty."
    else
        Match (fun cur ->
            let results =
                matches
                |> Seq.map(fun f -> result (f, cur))
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
        match cur.Item with
        | Item act ->
            if char = act then
                Success cur.Next
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
        Match (fun cur ->
            let r = result (str |> Seq.map matchChar |> Seq.reduce thenMatch, cur)
            match r with
            | Success _ -> r
            | Failure (msg, _) -> Failure (sprintf "Failure parsing '%s'. %s" str msg, cur))

let matchAnyStr (strings: seq<string>) =
    match strings with
    | empty when empty |> Seq.isEmpty ->
        success
    | _ ->
        strings |> Seq.map matchStr |> matchAny
