module Classier.NET.Compiler.Test

open Fuchu

let runTests =
    [
        ParserTest.tests
        AnalysisTest.tests
    ]
    |> testList "compiler tests"
    |> defaultMain

[<EntryPoint>]
let main args = runTests args
