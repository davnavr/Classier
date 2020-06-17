module Classier.NET.Compiler.Tests

open System.Reflection
open Classier.NET.Compiler.Assert
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
                    |> testsOfResult
                        [
                            testSuccess
                                "correct namespace"
                                (fun (cu, _) -> equal expectedNs cu.Namespace)

                            testSuccess
                                "correct usings"
                                (fun (cu, _) ->
                                    let useList =
                                        usings
                                        |> Seq.map Identifier.ofStrings
                                        |> List.ofSeq
                                    cu.Usings |> equal useList)

                            testSuccess
                                "types in symbol table"
                                (fun (_, state) ->
                                    state.Symbols
                                    |> GlobalsTable.getTypes expectedNs // TODO: Instead of mapping the GlobalsTable to a string seq, map the expected strings into placeholder defs.
                                    |> Seq.map
                                        (fun gtype ->
                                            gtype.Type
                                            |> GlobalType.getName
                                            |> string)
                                    |> isSuperSet defs)
                        ]
                    |> testList sourceName)
            |> testList "success tests"

            parseSource "HelloWorld"
            |> testSuccess
                "valid entry point"
                (fun (cu, _) -> cu.EntryPoint.IsSome |> isTrue "The entry point is missing")
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
