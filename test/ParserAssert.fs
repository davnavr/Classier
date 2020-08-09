module Classier.NET.Compiler.ParserAssert

open FParsec

let isSuccess =
    function
    | Success (result, state, _) ->
        result, state
    | Failure (msg, _, _) ->
        Assert.fail msg

let isFailure =
    function
    | Failure (_, err, _) -> err
    | Success (_, _, pos) ->
        Assert.failf "Unexpected success while parsing %s" pos.StreamName
