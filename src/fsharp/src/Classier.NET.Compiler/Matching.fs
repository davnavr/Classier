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

let isSuccess result =
    match result with
    | Success _ -> true
    | Failure _ -> false

let result<'T> (f: MatchFunc<'T>, cur) =
    let (Match matchFunc) = f
    matchFunc(cur)

let success<'T> =
    Match (fun (cur: Cursor<'T>) -> Success cur)

let failure<'T> msg =
    Match (fun (cur: Cursor<'T>) -> Failure(msg, cur))

// and
let thenMatch<'T> (m1: MatchFunc<'T>) m2: MatchFunc<'T> =
    Match (fun c1 ->
        let r1 = result (m1, c1)
        match r1 with
        | Failure _ -> r1
        | Success c2 -> result (m2, c2))

//let orMatch<'T>

let matchAny<'T> (matches: seq<MatchFunc<'T>>) = failure "Not implemented."

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
    match str with
    | empty when empty |> Seq.isEmpty -> success
    | _ ->
        Match (fun cur ->
            let r = result (str |> Seq.map matchChar |> Seq.reduce thenMatch, cur)
            match r with
            | Success _ -> r
            | Failure (msg, c) -> Failure (sprintf "Failure parsing '%s'. %s" str msg, c))

let matchAnyStr strings =
    match strings with
    | empty when empty |> Seq.isEmpty ->
        success
    | _ -> failure "Not implemented"
