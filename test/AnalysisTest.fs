[<RequireQualifiedAccess>]
module Classier.NET.Compiler.AnalysisTest

open System.Collections.Immutable
open Classier.NET.Compiler.SemAnalysis
open FParsec
open Fuchu

let testStrs name sources f =
    test name {
        let (result, state) =
            Seq.fold
                (fun (acc, state) source ->
                    match acc with
                    | Result.Ok list ->
                        let result =
                            runParserOnString
                                Parser.compilationUnit
                                Parser.defaultState
                                name
                                source
                        match result with
                        | Success(cu, nstate, _) ->
                            let nacc =
                                list
                                |> ImmList.add cu
                                |> Result.Ok
                            nacc, nstate
                        | Failure(msg, _, _) ->
                            Result.Error msg, state
                    | Result.Error _ -> acc, state)
                (Result.Ok ImmutableList.Empty, Parser.defaultState)
                sources
        Analyze.output
            (Assert.isOk result, state.EntryPoint)
            GlobalsTable.empty
        |> Assert.isOk
        |> ignore
    }

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

        ParserTest.testStr
            Parser.compilationUnit
            "empty nested interface is included"
            (fun parse ->
                let (cu, _) =
                    """
                    module Parent {
                        interface Child { }
                    }
                    """
                    |> parse
                    |> ParserAssert.isSuccess
                let output =
                    Analyze.output
                        (List.singleton cu, None)
                        GlobalsTable.empty
                    |> Assert.isOk
                let parent =
                    Seq.head output.GlobalTypes
                invalidOp "TODO: Check that the nested interface is there")
    ]
    |> testList "analysis tests"
