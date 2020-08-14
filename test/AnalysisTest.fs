[<RequireQualifiedAccess>]
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

let testStrs name gtable sources f =
    test name {
        let (result, state) = parseMany name sources
        Analyze.output
            (Assert.isOk result, state.EntryPoint)
            (gtable Globals.emptyTable)
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
            "types from all files are parsed"
            id
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
                output.GlobalTypes
                |> Seq.length
                |> Assert.equal 4)

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
                    Globals.emptyTable
                |> Assert.isError
                |> ImmList.exists
                    (function
                    | DuplicateGlobalSymbol _ -> true
                    | _ -> false)
                |> Assert.isTrue "Contains duplication error message"
                |> ignore)

        testStrs
            "nested type in a nested type is processed"
            id
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
                let m1 =
                    match Seq.head output.GlobalTypes with
                    | GenGlobalModule mdle -> Some mdle
                    | _ -> None
                    |> Option.get
                let m2 =
                    match m1.Members.Item 0 with
                    | (_, TypeOrMember.Type ntype) ->
                        match ntype with
                        | GenNestedModule mdle -> Some mdle
                        | _ -> None
                    | _ -> None
                    |> Option.get
                Assert.notEmpty m2.Members)
    ]
    |> testList "analysis tests"
