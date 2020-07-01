module Classier.NET.Compiler.Assert

let fail msg = msg |> Fuchu.AssertException |> raise

let isSome opt =
    match opt with
    | Some value -> value
    | None ->
        fail "The value was unexpectedly None"
