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
                "FancyClass", "", [ "System" ], [ "FancyClass", 4 ]
                "HelloWorld", "", [ "System.Console" ], List.empty
                "MethodOverloading", "", List.empty, [ "OverloadingExample", 6 ]
                "MultipleClasses", "test", List.empty, [ "Class1", 3; "Class2", 3; "Class3", 2; "Class4", 2; "Class5", 1; "Class6", 1; "Interface1", 1 ]
                "MyAbstractClass", "this.is.my.space", [ "java.lang"; "java.util" ], [ "MyAbstractClass", 4 ]
                "MyException1", "", List.empty, [ "MyException1", 3 ]
                "MyGenericClass", "some.name.collections", [ "blah.interop.clr.SomeClass"; "some.StaticClass<String>.Nested" ], [ "MutableList<T>", 3 ]
                "MyModule", "blah.blah.blah", [ "system.reflection.Assembly" ], [ "Math", 5 ]
                "NoAccessModifiers", "My.Awesome.Project", List.empty, [ "MyModule", 2; "MyModule", 1; "MyInterface", 1 ]
                "PropertyTest", "", List.empty, [ "IPropertyTest", 1; "PropertyTest", 5  ]
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
                                "types in symbol table"
                                (fun _ state ->
                                    let expectedNs =
                                        state.Symbols
                                        |> GlobalsTable.getNamespaces
                                        |> Seq.find (fun tablens -> string tablens = ns)
                                    let expectedTypes = Seq.map fst defs
                                    state.Symbols
                                    |> GlobalsTable.getTypes expectedNs
                                    |> Seq.map
                                        (fun gtype ->
                                            gtype.Type
                                            |> GlobalType.getName
                                            |> string)
                                    |> Assert.isSuperSet expectedTypes)

                            TestCase.psuccess
                                "member count"
                                (fun cu _ ->
                                    cu.Types
                                    |> Seq.map (snd >> TypeDef.getMembers >> Seq.length)
                                    |> List.ofSeq
                                    |> Assert.equal
                                        "member counts"
                                        (List.map snd defs))

                            TestCase.psuccess
                                "empty stacks"
                                (fun _ state ->
                                    [
                                        Assert.empty state.Members
                                        Assert.empty state.Params
                                    ]
                                    |> Assert.list)
                        ]
                    |> testList sourceName)
            |> testList "success tests"

            parseSource "HelloWorld"
            |> TestCase.psuccess
                "valid entry point"
                (fun cu _ -> cu.EntryPoint.IsSome |> Assert.isTrue "The entry point is missing")

            [
                "BadOverloadCtor", "already", 3L, 5L
                "BadOverloadInferredParamType", "exists", 8L, 5L
                "BadOverloadReturnType", "already exists", 6L, 5L
                "DuplicateEntryPoint", "existing entry point", 9L, 5L
                "DuplicateParamNames", "'p1' already", 3L, 3L
                "ParamConflictsWithSelfId", "'self' already exists", 6L, 5L
            ]
            |> Seq.collect
                (fun (source, err, lineNum, colNum) ->
                    parseSource source
                    |> TestCase.ofResult
                        [
                            TestCase.pfailure
                                source
                                (fun msg pErr ->
                                    [
                                        Assert.hasSubstring err msg
                                        Assert.equal "line number" lineNum pErr.Position.Line
                                        Assert.equal "column number" colNum pErr.Position.Column
                                    ]
                                    |> Assert.list)
                        ])
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
