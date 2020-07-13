[<RequireQualifiedAccess>]
module Classier.NET.Compiler.AnalysisTest

open System.Collections.Immutable
open Classier.NET.Compiler.IR
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
        |> f
        |> ignore
    }

let tests =
    [
        testStrs
            "valid types are returned as result"
            [
                """
                class Classy {
                }

                interface IAmSomething { }

                module Maths
                {
                }
                """
            ]
            (fun result ->
                result.GlobalTypes
                |> Seq.map (snd >> GenType.name >> string)
                |> List.ofSeq
                |> Assert.equal
                    [
                        "Classy"
                        "IAmSomething"
                        "Maths"
                    ])

        testStrs
            "empty nested interface is included"
            [
                """
                module Parent {
                    interface Child { }
                }
                """
            ]
            (fun output ->
                let parent =
                    match Assert.head output.GlobalTypes with
                    | (_, GenModule mdle) -> mdle
                    | _ ->
                        Assert.fail "The parent module does not exist"
                match Assert.head parent.Members with
                | (_, TypeOrMember.Type ntype) -> ntype
                | _ -> Assert.fail "Expected the nested child type."
                |> GenType.name
                |> string
                |> Assert.equal "Child")
    ]
    |> testList "analysis tests"
