module Classier.NET.Compiler.TestCase

open FParsec
open Fuchu

let presult name tsuccess tfailure result =
    fun _ ->
        match result with
        | Success (success, state, _) -> tsuccess success state
        | Failure (msg, err, _) -> tfailure msg err
        |> Assert.run
    |> testCase name
let psuccess name test =
    presult
        name
        test
        (fun msg _ -> Assert.fail msg) 
let pfailure name =
    presult
        name
        (fun _ _ -> Assert.fail "Unexpected success")

let ofResult tests result =
    Seq.map
        (fun (test: 'a -> Test) -> test result)
        tests
