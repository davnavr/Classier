[<RequireQualifiedAccess>]
module Classier.NET.Compiler.AnalysisTest

open Classier.NET.Compiler.SemAnalysis
open Fuchu

let tests =
    [
        ParserTest.testStr
            Parser.compilationUnit
            "valid types are returned as result"
            (fun parse ->
                let (cu, _) =
                    """
                    class Global1 {
                    }

                    interface Global2 { }

                    module Global3
                    {
                    }
                    """
                    |> parse
                    |> ParserAssert.isSuccess
                Analyze.output
                    (List.singleton cu, None)
                    GlobalsTable.empty
                |> Assert.isOk)
    ]
    |> testList "analysis tests"
