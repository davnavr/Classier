[<RequireQualifiedAccess>]
module Classier.NET.Compiler.ParserTest

open Fuchu
open FParsec

open Classier.NET.Compiler.TypeSystem

open Classier.NET.Compiler.Grammar

let parseStr parser name f =
    runParserOnString
        parser
        Parser.defaultState
        name
    |> f

let testStr parser name f =
    test name {
        parseStr
            parser
            name
            f
        |> ignore
    }

let tests =
    [
        testStr
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

        testStr
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

                    public abstract class Class4<T, U implements IThing>;
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

        testStr
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
                        }
                        catch {
                            _ => { };
                        }
                        finally {
                        }

                        while (false) {
                        }

                        match (thing) {
                            _ => { };
                        }

                        "Hello"
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

                        { Choice1 = List.empty
                          Choice2 = List.empty
                          Condition = BoolLit true }
                        |> IfExpr
                        |> IgnoredExpr

                        { Finally = List.empty
                          Handlers = [ Expression.emptyCase ]
                          TryBody = List.empty }
                        |> TryExpr
                        |> IgnoredExpr

                        While (BoolLit false, List.empty)

                        { Against =
                            Identifier.create "thing"
                            |> Option.get
                            |> Identifier.ofStr
                            |> IdentifierRef
                          Cases = [ Expression.emptyCase ] }
                        |> MatchExpr
                        |> IgnoredExpr

                        StrLit "Hello" |> Return
                    ])

        parseStr
            (Parser.expression .>> eof)
            "test expression"
            (fun parse ->
                [
                    "\"h\u00E9llo\""
                    "12345"
                    "67890L"
                    "54321l"
                    "3.14159265"
                    "2.71828f"
                    "1.2345D"
                    "\"hello world\" |> System.Console.WriteLine"
                    "1 + 2"
                    "(a: int, b: float, c: string[]) => {\n    return c;\n}"
                ]
                |> Seq.map (fun source ->
                    test source {
                        source
                        |> parse
                        |> ParserAssert.isSuccess
                        |> ignore
                    })
                |> testList "expression is valid")

        testStr
            Parser.compilationUnit
            "class can have self-identifier"
            (fun parse ->
                let (cu, _) =
                    """
                    /*
                    Copyright (c) 2020
                    This is a very long text
                    */

                    public class FancyClass() as this {
                        let myField = this.toString();
                    }
                    """
                    |> parse
                    |> ParserAssert.isSuccess
                let fclass =
                    cu.Types
                    |> List.head
                    |> snd
                    |> function
                    | Class cdef -> cdef
                    | _ -> Assert.fail "The class was unexpectedly missing"
                fclass.SelfIdentifier
                |> Assert.isSome
                |> string
                |> Assert.equal "this")

        parseStr
            (Parser.typeName .>> eof)
            "test type name"
            (fun parse ->
                let tidentifier names =
                    names
                    |> Seq.map
                        (Identifier.create >> Option.get)
                    |> Identifier.ofStrSeq
                    |> Option.get
                    |> Type.Named

                [
                    Primitive PrimitiveType.String |> ArrayType

                    tidentifier [ "java"; "lang"; "Object" ] |> ArrayType |> ArrayType

                    Primitive PrimitiveType.Null
                    Primitive PrimitiveType.Unit
                    Primitive PrimitiveType.Byte
                    Primitive PrimitiveType.Short
                    Primitive PrimitiveType.Long
                    Primitive PrimitiveType.Double

                    tidentifier [ "System"; "Object" ]

                    Tuple [ Primitive PrimitiveType.Int; Primitive PrimitiveType.Float ]
                ]
                |> Seq.map (fun exp ->
                    let ename = string exp
                    test ename {
                        parse ename
                        |> ParserAssert.isSuccess
                        |> fst
                        |> Assert.equal (TypeName exp)
                        |> ignore
                    }))
        |> testList "simple type name is valid"

        testStr
            Parser.compilationUnit
            "more than one entry point is not allowed"
            (fun parse ->
                """
                main() {
                    "One" |> System.Console.WriteLine;
                }

                main() {
                    "Tw\u00D0"
                }
                """
                |> parse
                |> ParserAssert.isFailure
                |> string
                |> Assert.strContains "entry point already exists")

        testStr
            (Parser.statement .>> eof)
            "let can declare local functions"
            (fun parse ->
                let efunc =
                    let pmap =
                        List.map (fun (name, ptype) ->
                            let pname =
                                name
                                |> Identifier.create
                                |> Option.get
                                |> Some
                            Param.create (TypeName ptype) pname)
                    let name =
                        VarPattern(Identifier.create "myLocal" |> Option.get, None)
                    let func =
                        { Body =
                            List.zip
                                [
                                    Position("let can declare local functions", 76L, 2L, 24L)
                                ]
                                [
                                    let temp =
                                        Identifier.create
                                        >> Option.get
                                        >> Identifier.ofStr
                                    let one =
                                        MemberAccess(temp "arg1" |> IdentifierRef, temp "length")
                                    let two = temp "arg2" |> IdentifierRef
                                    InfixOp(InfixOp(one, "+", two), "+", temp "arg3" |> IdentifierRef)
                                    |> Return
                                ]
                          Parameters =
                            [
                                [ "arg1", Primitive PrimitiveType.String; "arg2", Primitive PrimitiveType.Int ]
                                [ "arg3", Primitive PrimitiveType.Int ]
                            ]
                            |> List.map pmap
                          ReturnType = None }
                        |> AnonFunc
                    LetDecl(name, func)
                """let myLocal (arg1: string, arg2: int) (arg3: int) {
                       arg1.length + arg2 + arg3
                   }"""
                |> parse
                |> ParserAssert.isSuccess
                |> fst
                |> snd
                |> Assert.equal efunc)

        testStr
            (Parser.statement .>> eof)
            "var can be used without a value"
            (fun parse ->
                let edecl =
                    let name =
                        "myMutable"
                        |> Identifier.create
                        |> Option.get
                    let vtype =
                        Primitive PrimitiveType.Double
                        |> TypeName
                        |> Some
                    VarDecl(VarPattern(name, vtype), None)
                "var myMutable: double;"
                |> parse
                |> ParserAssert.isSuccess
                |> fst
                |> snd
                |> Assert.equal edecl)

        parseStr
            Parser.compilationUnit
            "namespace test"
            (fun parse ->
                [
                    "// empty", Namespace List.empty
                    "namespace hello;", Namespace List.empty // TODO: Expected should not be empty for these three.
                    "namespace System.Collections ;", Namespace List.empty
                    "namespace some.random.long.namespace;", Namespace List.empty
                ]
                |> Seq.mapi (fun i (ns, exp) ->
                    test (string i) {
                        let (cu, _) =
                            sprintf
                                """
                                %s

                                public class Hello { }
                                """
                                ns
                            |> parse
                            |> ParserAssert.isSuccess
                        Assert.equal
                            exp
                            cu.Namespace
                        |> ignore
                    })
                |> testList "correct namespace is parsed")
    ]
    |> testList "parser tests"
