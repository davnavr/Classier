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
            sources
            |> Seq.indexed
            |> Seq.fold
                (fun (acc, state) (i, source) ->
                    match acc with
                    | Result.Ok list ->
                        let result =
                            runParserOnString
                                Parser.compilationUnit
                                Parser.defaultState
                                (sprintf "%s%i" name i)
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
            "global usings are validated"
            [
                """
                namespace my.fancy.space;
                class MyClass { }
                """
                """
                class Global {
                  class Nested { }
                }
                """
                """
                // This is the one that should be checked.
                namespace other.space;

                use my.fancy.space;
                use my.fancy.space.MyClass;
                use Global;
                use Nested;

                class IgnoreMe { }
                """
            ]
            (fun _ -> ()) // TODO: Find something to check in this test.

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
                |> Seq.map (GenType.gname >> string)
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
                    | GenGlobalModule mdle -> mdle
                    | _ ->
                        Assert.fail "The parent module does not exist"
                match Assert.head parent.Members with
                | (_, TypeOrMember.Type ntype) -> ntype
                | _ -> Assert.fail "Expected the nested child type."
                |> GenType.nname
                |> string
                |> Assert.equal "Child")

        testStrs
            "entry point exists"
            [
                "main (args: string[]) { }"
            ]
            (fun output ->
                let epoint = Option.get output.EntryPoint
                Assert.equal
                    EntryPointReturn.ImplicitZero
                    epoint.Body.ReturnType)
    ]
    |> testList "analysis tests"
