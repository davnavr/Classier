module Classier.NET.Compiler.Assert

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

let notEmpty col =
    if Seq.isEmpty col
    then raise (AssertException "The collection was unexpectedly empty")
    else ()
