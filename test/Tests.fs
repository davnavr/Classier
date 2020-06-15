module Classier.NET.Compiler.Tests

open System.Reflection
open Classier.NET.Compiler.Assert
open FParsec
open Fuchu

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
            "MultipleClasses", [ "test" ], List.empty
            "MyAbstractClass", [ "this"; "is"; "my"; "space" ], [ [ "java"; "lang" ]; [ "java"; "util" ] ]
        ]
        |> Seq.map
            (fun (sourceName, ns, usings) ->
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
                                    |> Seq.map (Identifier.ofStrings >> Option.get)
                                    |> List.ofSeq
                                cu.Usings |> equal useList)

                        testSuccess
                            "namespace in symbol table"
                            (fun (_, state) ->
                                state.Symbols
                                |> GlobalsTable.getTypes expectedNs
                                |> Assert.notEmpty)
                    ]
                |> testList sourceName)
        |> testList "success tests"
    ]
    |> testList "parser tests"
    |> run
