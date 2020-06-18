module Classier.NET.Compiler.Assert

open System

type AssertionResult =
    | Success
    | Failure of string
    | Aggregate of AssertionResult list

type AssertException = Fuchu.AssertException

type Assertion = Assertion of (unit -> AssertionResult)

let getResult (Assertion a) = a()

let run (Assertion a) =
    match a() with
    | Success -> ()
    | Failure msg -> msg |> AssertException |> raise
    | Aggregate results ->
        let errors =
            results
            |> Seq.collect
                (fun result ->
                    match result with
                    | Aggregate more -> Seq.ofList more
                    | _ -> Seq.singleton result)
            |> Seq.choose
                (fun result ->
                    match result with
                    | Failure msg -> Some msg
                    | _ -> None)
            |> Seq.toList
        match errors.Length with
        | 0 -> ()
        | _ ->
            errors
            |> String.concat "\n"
            |> sprintf "%i of %i assertions failed:\n%s"
                errors.Length
                results.Length
            |> AssertException
            |> raise

let list alist =
    fun() ->
        match alist with
        | [] -> Failure "The assertion list was unexpectedly empty"
        | _ ->
            alist
            |> List.map getResult
            |> Aggregate
    |> Assertion

let fail msg = Assertion (fun() -> Failure msg)

let equalM expected actual msg =
    fun () ->
        if expected = actual
        then Success
        else Failure msg
    |> Assertion
let equal name expected actual =
    sprintf
        "%s are not equal\nExpected:\n%A\nActual:\n%A"
        name
        expected
        actual
    |> equalM expected actual

let isTrue msg value = equalM true value msg

let notEmpty col =
    Seq.isEmpty col
    |> not
    |> isTrue "The collection was unexpectedly empty"

let isSuperSet other col =
    let missing = other |> Seq.except col
    let msg =
        String.Join<_>(", ", missing)
        |> sprintf "The set is missing the following elements: %s"
    missing |> Seq.isEmpty |> isTrue msg

let hasSubstring (sub: string) (full: string) =
    let msg =
        sprintf
            "Expected substring:\n%s\nActual string:\n%s"
            sub
            full
    isTrue msg (full.Contains sub)
