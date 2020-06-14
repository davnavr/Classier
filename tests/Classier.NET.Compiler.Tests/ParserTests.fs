module Classier.NET.Compiler.ParserTests

open Fuchu

[<Tests>]
let tests =
    [
        test "this is a test" {
            Assert.Equal("test message", 4, 4);
        }
    ]
    |> testList ""
