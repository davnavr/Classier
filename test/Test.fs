module Classier.NET.Compiler.Test

open Fuchu

let runTests =
    [
    ]
    |> testList "compiler tests"
    |> defaultMain

[<EntryPoint>]
let main args = runTests args
