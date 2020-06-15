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
    let testFilter (name: string) =
        match args with
        | [||] -> true
        | _ ->
            args
            |> Seq.fold
                (fun found arg ->
                    found || name.StartsWith arg)
                false

    [
        [
            [
                "MethodOverloading", List.empty, List.empty
                "MultipleClasses", [ "test" ], List.empty
                "MyAbstractClass", [ "this"; "is"; "my"; "space" ], [ [ "java"; "lang" ]; [ "java"; "util" ] ]
                "MyModule", List.replicate 3 "blah", [ [ "system"; "reflection"; "Assembly" ] ]
                "NoAccessModifiers", [ "My"; "Awesome"; "Project" ], List.empty
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
                                        |> Seq.map Identifier.ofStrings
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
    ]
    //|> Seq.map (filter testFilter)
    |> testList "compiler tests"
    |> filter testFilter
    |> run
