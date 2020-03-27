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

let result<'T> (f: MatchFunc<'T>) cur =
    let (Match matchFunc) = f
    matchFunc(cur)

let andThen<'T> (m1: MatchFunc<'T>) m2: MatchFunc<'T> =
    Match (fun c1 ->
        let r1 = result m1 c1
        match r1 with
        | Failure _ -> r1
        | Success c2 -> result m2 c2)
