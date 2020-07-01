module Classier.NET.Compiler.ParserTest

open Classier.NET.Compiler.Grammar
open FParsec
open Fuchu

let parseStr parser name f =
    testCase name (fun() ->
        runParserOnString
            parser
            Parser.defaultState
            name
        |> f
        |> ignore)

let tests =
    [
        parseStr
            Parser.compilationUnit
            "hello world"
            (fun parse ->
                let (_, state) =
                    """
                    use System.Console;

                    main() {
                        WriteLine("Hello there!");
                        0
                    }
                    """
                    |> parse
                    |> ParserAssert.isSuccess
                state.EntryPoint
                |> Assert.isSome)

        parseStr
            Parser.compilationUnit
            "types and modules in compilation unit"
            (fun parse ->
                let (cu, _) =
                    """
                    public class Class1 {
                    }

                    public data class Class2(thing1: int);

                    public interface IThing {
                    }

                    public module Module1 {
                    }

                    public class Class3<T> protected {
                    }

                    public abstract class Class4<T, U implements IThing> {
                    }
                    """
                    |> parse
                    |> ParserAssert.isSuccess
                cu.Types
                |> Seq.map (fun (_, tdef) ->
                    tdef
                    |> TypeDef.name
                    |> string)
                |> List.ofSeq
                |> Assert.equal
                    [
                        "Class1"
                        "Class2"
                        "IThing"
                        "Module1"
                        "Class3<T>"
                        "Class4<T, U>"
                    ])

        parseStr
            Parser.statementBlock
            "block statements don't need semicolon"
            (fun parse ->
                let (statements, _) =
                    """{
                        3.14159265; // This ignored expression needs a semicolon.
                        if (true) {
                        }
                        else {
                        }

                        try {
                            throw "Hello!";
                        }
                        catch {
                            _ => throw;
                        }
                        finally {
                        }

                        while (true) {
                        }

                        match (thing) {
                            1 => "yes";
                            (one, two) => oh.no;
                        }
                    }"""
                    |> parse
                    |> ParserAssert.isSuccess
                statements
                |> Seq.map snd
                |> List.ofSeq
                |> Assert.equal
                    [
                        { FracDigits = "14159265"
                          IntDigits = "3" }
                        |> NumericLit.FPoint
                        |> NumLit
                        |> IgnoredExpr


                    ])
    ]
    |> testList "parser tests"
