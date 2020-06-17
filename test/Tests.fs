module Classier.NET.Compiler.Tests

open System.Reflection
open FParsec
open Fuchu
open Fuchu.Test

let parseSource source =
    use file =
        source
        |> sprintf "Classier.NET.Compiler.%s.txt"
        |> Assembly.GetExecutingAssembly().GetManifestResourceStream

    runParserOnStream
        Parser.compilationUnit
        ParserState.defaultState
        source
        file
        System.Text.Encoding.UTF8

[<EntryPoint>]
let main args =
    [
        [
            [
                "FancyClass", List.empty, [ [ "System" ] ], [ "FancyClass" ]
                "HelloWorld", List.empty, [ [ "System"; "Console" ] ], List.empty
                "MethodOverloading", List.empty, List.empty, [ "OverloadingExample" ]
                "MultipleClasses", [ "test" ], List.empty, [ "Class1"; "Class2"; "Interface1"; "Class3"; "Class4"; "Class5"; "Class6" ]
                "MyAbstractClass", [ "this"; "is"; "my"; "space" ], [ [ "java"; "lang" ]; [ "java"; "util" ] ], [ "MyAbstractClass" ]
                "MyModule", List.replicate 3 "blah", [ [ "system"; "reflection"; "Assembly" ] ], [ "Math" ]
                "NoAccessModifiers", [ "My"; "Awesome"; "Project" ], List.empty, [ "MyModule"; "MyModule"; "MyInterface" ]
            ]
            |> Seq.map
                (fun (sourceName, ns, usings, defs) ->
                    let expectedNs = Identifier.ofStrings ns

                    parseSource sourceName
                    |> TestCase.ofResult
                        [
                            TestCase.psuccess
                                "correct namespace"
                                (fun cu _ ->
                                    Assert.equal
                                        "namespaces"
                                        expectedNs
                                        cu.Namespace)

                            TestCase.psuccess
                                "correct usings"
                                (fun cu _ ->
                                    let useList =
                                        usings
                                        |> Seq.map Identifier.ofStrings
                                        |> List.ofSeq
                                    cu.Usings
                                    |> Assert.equal
                                        "usings"
                                        useList)

                            TestCase.psuccess
                                "types in symbol table"
                                (fun _ state ->
                                    state.Symbols
                                    |> GlobalsTable.getTypes expectedNs // TODO: Instead of mapping the GlobalsTable to a string seq, map the expected strings into placeholder defs.
                                    |> Seq.map
                                        (fun gtype ->
                                            gtype.Type
                                            |> GlobalType.getName
                                            |> string)
                                    |> Assert.isSuperSet defs)
                        ]
                    |> testList sourceName)
            |> testList "success tests"

            parseSource "HelloWorld"
            |> TestCase.psuccess
                "valid entry point"
                (fun cu _ -> cu.EntryPoint.IsSome |> Assert.isTrue "The entry point is missing")

            parseSource "DuplicateEntryPoint"
            |> TestCase.pfailure
                "duplicate entry point"
                (fun msg err ->
                    [
                        Assert.hasSubstring "existing entry point" msg
                        Assert.equal "line numbers" 9L err.Position.Line
                        Assert.equal "column numbers" 5L err.Position.Column
                    ]
                    |> Assert.list)
        ]
        |> testList "parser tests"
    ]
    |> testList "compiler tests"
    |> filter
        (fun name ->
            match args with
            | [||] -> true
            | _ ->
                args
                |> Seq.fold
                    (fun found arg -> found || name.StartsWith arg)
                    false)
    |> run
