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
        None
        source
        file
        System.Text.Encoding.UTF8

[<EntryPoint>]
let main args =
    [
        [
            [
                "FancyClass", "", [ "System" ], [ "FancyClass", 3 ]
                "HelloWorld", "", [ "System.Console" ], List.empty
                "MethodOverloading", "", List.empty, [ "OverloadingExample", 5 ]
                "MultipleClasses", "test", List.empty, [ "Class1", 2; "Class2", 2; "Interface1", 1; "Class3", 1; "Class4", 1; "Class5", 1; "Class6", 0 ]
                "MyAbstractClass", "this.is.my.space", [ "java.lang"; "java.util" ], [ "MyAbstractClass", 3 ]
                "MyException1", "", List.empty, [ "MyException1", 2 ]
                "MyGenericClass", "some.name.collections", [ "blah.interop.clr.SomeClass"; "some.StaticClass<String>.Nested" ], [ "MutableList<T>", 2 ]
                "MyModule", "blah.blah.blah", [ "system.reflection.Assembly" ], [ "Math", 5 ]
                "NoAccessModifiers", "My.Awesome.Project", List.empty, [ "MyModule", 1; "MyModule", 1; "MyInterface", 1 ]
                "PropertyTest", "", List.empty, [ "PropertyTest", 4 ; "IPropertyTest", 1; ]
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
                                    |> Namespace.fullId
                                    |> string
                                    |> Assert.equal "namespaces" ns)

                            TestCase.psuccess
                                "correct usings"
                                (fun cu _ ->
                                    cu.Usings
                                    |> List.map string
                                    |> Assert.equal "usings" usings)

                            TestCase.psuccess
                                "member count"
                                (fun cu _ ->
                                    cu.Types
                                    |> Seq.map (snd >> TypeDef.getMembers >> Seq.length)
                                    |> List.ofSeq
                                    |> Assert.equal
                                        "member counts"
                                        (List.map snd defs))
                        ]
                    |> testList sourceName)
            |> testList "success tests"
            
            [
                parseSource "HelloWorld"
                |> TestCase.psuccess
                    "valid entry point"
                    (fun cu _ -> cu.EntryPoint.IsSome |> Assert.isTrue "The entry point is missing")

                parseSource "DuplicateEntryPoint"
                |> TestCase.pfailure
                    "existing entry point"
                    (fun msg _ ->
                        Assert.hasSubstring
                            "entry point already exists at (\"DuplicateEntryPoint\", Ln: 4, Col: 1)"
                            msg)
            ]
            |> testList "failure tests"
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
