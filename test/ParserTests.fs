open Fuchu

do
    [
        test "this is a test" {
            Assert.Equal("test message", 4, 4);
        }
    ]
    |> testList "my list"
    |> run
    |> ignore
