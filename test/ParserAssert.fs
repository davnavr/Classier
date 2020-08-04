module Classier.NET.Compiler.ParserAssert

open FParsec

let isSuccess result =
    match result with
    | Success (result, state, _) ->
        result, state
    | Failure (msg, _, _) ->
        Assert.fail msg
