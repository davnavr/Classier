module Classier.NET.Compiler.AnalysisTest

open Classier.NET.Compiler.Globals
open Classier.NET.Compiler.SemAnalysis
open Classier.NET.Compiler.ToSource
open Fuchu

let tests =
    [
        [
            List.empty
            [ "hello" ]
            [ "my"; "library"; "collections" ]
        ]
        |> Seq.map (fun names ->
            let ns =
                names
                |> List.map (Identifier.create >> Option.get)
                |> Namespace
            let nsstr = string ns
            ParserTest.parseStr
                Parser.compilationUnit
                nsstr
                (fun parse ->
                    let (cu, _) =
                        """
                        class MyClass {}
                        interface MyInterface {}
                        module MyModule {}
                        """
                        |> parse
                        |> ParserAssert.isSuccess
                    let ptnames =
                        cu.Types
                        |> Seq.map (fun (_, tdef ) ->
                            let name =
                                Grammar.TypeDef.name tdef
                            name.Identifier)
                        |> List.ofSeq
                    let result =
                        GlobalsAnalyzer.analyze
                            GlobalsTable.empty
                            [ cu ]
                    [
                        test "types are valid" {
                            let gtnames =
                                result.Valid
                                |> Seq.map (GenType.name)
                                |> List.ofSeq
                            Assert.equal gtnames ptnames
                            |> ignore
                        }

                        test "types added to globals table" {
                            result.Table
                            |> GlobalsTable.getTypes ns
                            |> Seq.map (fun gtype ->
                                match gtype.Type with
                                | DefinedType (_, dtype) -> GenType.name dtype
                                | ExternType etype -> EType.name etype)
                            |> List.ofSeq
                            |> Assert.equal ptnames
                            |> ignore
                        }
                    ]
                    |> testList nsstr))
        |> testList "global analysis"
    ]
    |> testList "analysis tests"
