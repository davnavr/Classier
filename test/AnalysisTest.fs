[<RequireQualifiedAccess>]
module Classier.NET.Compiler.AnalysisTest

open System.Collections.Immutable
open FParsec

open Fuchu

open Classier.NET.Compiler.Extern
open Classier.NET.Compiler.IR
open Classier.NET.Compiler.SemAnalysis

let private parseMany name sources =
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

let testStrs name gtable sources f =
    test name {
        let (result, state) = parseMany name sources
        Analyze.output
            (Assert.isOk result, state.EntryPoint)
            (gtable GlobalsTable.empty)
        |> Assert.isOk
        |> f
        |> ignore
    }

let tests =
    [
        testStrs
            "global usings are validated"
            id
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
            id
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
            id
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
            id
            [
                "main (args: string[]) { }"
            ]
            (fun output ->
                let epoint = Option.get output.EntryPoint
                Assert.equal
                    EntryPointReturn.ImplicitZero
                    epoint.Body.ReturnType)

        testStrs
            "hello world is a valid program"
            Program.TempStandardLib.table
            [
                """
                main (args: string[]) {
                    System.Console.WriteLine("Hello World!");
                }
                """
            ]
            (fun output ->
                let epoint = Option.get output.EntryPoint
                Assert.notEmpty epoint.Body.Statements)

        testCase
            "namespace and type cannot have the same name"
            (fun() ->
                let (result, state) =
                    [
                        """
                        namespace MyBadName;

                        class MyClass;
                        """
                        """
                        class MyBadName { }
                        """
                    ]
                    |> parseMany "duplicate name"
                Analyze.output
                    (Result.get result, state.EntryPoint)
                    GlobalsTable.empty
                |> Assert.isError
                |> ImmList.exists
                    (function
                    | DuplicateGlobalSymbol _ -> true
                    | _ -> false)
                |> Assert.isTrue "Contains duplication error message"
                |> ignore)

    ]
    |> testList "analysis tests"
