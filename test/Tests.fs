module Classier.NET.Compiler.Tests

open System.Reflection
open Classier.NET.Compiler.Grammar
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
                "FancyClass", "", [ "System" ], [ "FancyClass" ]
                "HelloWorld", "", [ "System.Console" ], List.empty
                "MethodOverloading", "", List.empty, [ "OverloadingExample" ]
                "MultipleClasses", "test", List.empty, [ "Class1"; "Class2"; "Interface1"; "Class3"; "Class4"; "Class5"; "Class6" ]
                "MyAbstractClass", "this.is.my.space", [ "java.lang"; "java.util" ], [ "MyAbstractClass" ]
                "MyException1", "", List.empty, [ "MyException1" ]
                "MyGenericClass", "some.name.collections", [ "blah.interop.clr.SomeClass"; "some.StaticClass<String>.Nested" ], [ "MutableList<T>" ]
                "MyModule", "blah.blah.blah", [ "system.reflection.Assembly" ], [ "Math" ]
                "NoAccessModifiers", "My.Awesome.Project", List.empty, [ "MyModule"; "MyModule"; "MyInterface" ]
                "PropertyTest", "", List.empty, [ "PropertyTest" ]
            ]
            |> Seq.map
                (fun (sourceName, ns, usings, defs) ->
                    parseSource sourceName
                    |> TestCase.ofResult
                        [
                            TestCase.psuccess
                                "correct namespace"
                                (fun cu _ ->
                                    cu.Namespace
                                    |> string
                                    |> Assert.equal "namespaces" ns)

                            TestCase.psuccess
                                "correct usings"
                                (fun cu _ ->
                                    cu.Usings
                                    |> List.map string
                                    |> Assert.equal "usings" usings)

                            TestCase.psuccess
                                "types in symbol table"
                                (fun _ state ->
                                    let expectedNs =
                                        state.Symbols
                                        |> GlobalsTable.getNamespaces
                                        |> Seq.find (fun tableNs -> string tableNs = ns)
                                    state.Symbols
                                    |> GlobalsTable.getTypes expectedNs
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
