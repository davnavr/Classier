﻿[<RequireQualifiedAccess>]
module Classier.NET.Compiler.AnalysisTest

open System.Collections.Immutable
open FParsec

open Fuchu

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

let testStrs name sources f =
    test name {
        let (result, state) = parseMany name sources
        (Result.get result, state.EntryPoint)
        |> Analyze.output
        |> Result.get
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
            "types from all files are parsed"
            [
                """
                class Class1;
                """
                """
                internal class Class2 { }
                """
                """
                public module List { }
                """
                """
                namespace Working;

                public class Maybe();
                """
            ]
            (fun output ->
                output.GlobalDecls
                |> Seq.length
                |> Assert.equal 4)

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
                result.GlobalDecls
                |> Seq.map (GenDecl.gname >> string)
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
                    match Assert.head output.GlobalDecls with
                    | GenGlobalModule mdle -> mdle
                    | _ ->
                        Assert.fail "The parent module does not exist"
                match Assert.head parent.Members with
                | (_, NestedDecl ndecl) -> ndecl
                | _ -> Assert.fail "Expected the nested child type."
                |> GenDecl.nname
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
                    ExitCode.ImplicitZero
                    epoint.ExitCode)

        testStrs
            "hello world is a valid program"
            [
                """
                namespace System;

                extern module Console {
                    def WriteLine(value: string): ();
                }
                """

                """
                main (args: string[]) {
                    System.Console.WriteLine("Hello World!");
                }
                """
            ]
            (fun output ->
                let epoint = Option.get output.EntryPoint
                Assert.notEmpty epoint.Body)

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
                (Result.get result, state.EntryPoint)
                |> Analyze.output
                |> Assert.isError
                |> ImmList.exists
                    (function
                    | DuplicateGlobalSymbol _ -> true
                    | _ -> false)
                |> Assert.isTrue "Contains duplication error message"
                |> ignore)

        testStrs
            "nested type in a nested type is processed"
            [
                """
                module Table {
                    module Inner {
                        class Symbol(name: string);
                    }
                }
                """
            ]
            (fun output ->
                match Seq.head output.GlobalDecls with
                | GenGlobalModule { Members = members } ->
                    match members.Item 0 with
                    | (_, NestedDecl (GenNestedModule { Members = nmembers })) ->
                        Some nmembers
                    | _ -> None
                | _ -> None
                |> Option.get
                |> Assert.notEmpty)
    ]
    |> testList "analysis tests"
