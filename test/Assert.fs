module Classier.NET.Compiler.Assert

let inline fail msg = msg |> Fuchu.AssertException |> raise
let inline failf format = Printf.ksprintf fail format

let isSome opt =
    match opt with
    | Some value -> value
    | None ->
        fail "The value was unexpectedly None"

let equal exp act =
    match exp = act with
    | true -> act
    | false ->
        failf
            "Expected:\n%O\nActual:\n%O"
            exp
            act
