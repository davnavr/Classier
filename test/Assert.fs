module Classier.NET.Compiler.Assert

open System.Linq
open FParsec
open Fuchu

let testSuccess name test result =
    fun _ ->
        match result with
        | Success (success, state, _) -> test (success, state)
        | Failure (msg, _ ,_) -> raise (AssertException msg)
    |> testCase name 

let equal expected actual =
    let msg =
        sprintf
            "%A should be equal to %A"
            actual
            expected

    Assert.Equal(msg, expected, actual)

let testsOfResult tests result = Seq.map (fun (test: 'a -> Test) -> test result) tests

let isTrue msg value =
    if value
    then ()
    else raise (AssertException msg)

let notEmpty col =
    Seq.isEmpty col
    |> not
    |> isTrue "The collection was unexpectedly empty"

let isSuperSet other col =
    let missing = Enumerable.Except(other, col)
    let msg =
        missing
        |> String.concat ", "
        |> sprintf "The set is missing the following elements: %s"

    missing.Any() |> not |> isTrue msg
