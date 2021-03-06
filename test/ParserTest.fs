﻿[<RequireQualifiedAccess>]
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

let strEqual parser name str map expected =
    testStr
        parser
        name
        (fun parse ->
            str
            |> parse
            |> ParserAssert.isSuccess
            |> fst
            |> map
            |> Assert.equal expected)
let strSuccess parser name str =
    testStr
        parser
        name
        (fun parse ->
            str
            |> parse
            |> ParserAssert.isSuccess)
let strFailure parser name str errsub =
    testStr
        parser
        name
        (fun parse ->
            str
            |> parse
            |> ParserAssert.isFailure
            |> string
            |> Assert.strContains errsub)

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
                Assert.isSome state.EntryPoint)

        testStr
            Parser.compilationUnit
            "defined types and modules in compilation unit have correct names"
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
                cu.Declarations
                |> Seq.map (Declaration.name >> string)
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

        strEqual
            Parser.statementBlock
            "block statements don't need semicolon"
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
            (List.map snd)
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
                    Identifier.create "thing" |> IdentifierRef
                  Cases = [ Expression.emptyCase ] }
                |> MatchExpr
                |> IgnoredExpr

                StrLit "Hello" |> Return
            ]

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
                    cu.Declarations
                    |> List.head
                    |> function
                    | Declaration.Defined(_, Class cdef) -> cdef
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
                    |> Seq.map IdentifierStr.create
                    |> FullIdentifier.ofStrs
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

                    Tuple(Primitive PrimitiveType.Int, Primitive PrimitiveType.Float, List.empty)
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

        strFailure
            Parser.compilationUnit
            "more than one entry point is not allowed"
            """
            main() {
                "One" |> System.Console.WriteLine;
            }

            main() {
                "Tw\u00D0"
            }
            """
            "entry point already exists"

        testStr
            (Parser.statement .>> eof)
            "let can declare local functions"
            (fun parse ->
                let efunc =
                    let pmap =
                        List.map (fun (name, ptype) ->
                            let pname =
                                name
                                |> IdentifierStr.create
                                |> Some
                            Param.create (TypeName ptype) pname)
                    let name =
                        VarPattern(IdentifierStr.create "myLocal", None)
                    let func =
                        { Body =
                            List.zip
                                [
                                    Position("let can declare local functions", 76L, 2L, 24L)
                                ]
                                [
                                    let one = Identifier.create "arg1" |> IdentifierRef
                                    let two = Identifier.create "arg2" |> IdentifierRef
                                    let three = Identifier.create "arg3" |> IdentifierRef
                                    let n1 = MemberAccess(one, Identifier.create "length")
                                    let n2 = InfixOp(n1, "+", two)

                                    InfixOp(n2, "+", three) |> Return
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
                        IdentifierStr.create "myMutable"
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
                    let nsname =
                        List.map IdentifierStr.create >> Namespace

                    "// empty", Namespace List.empty
                    "namespace hello;", nsname [ "hello" ]
                    "namespace System.Collections ;", nsname [ "System"; "Collections" ]
                    "namespace some.random.long.namespace;", nsname [ "some"; "random"; "long"; "namespace" ]
                ]
                |> Seq.mapi (fun i (ns, exp) ->
                    test (string i) {
                        let (cu, _) =
                            sprintf
                                """
                                %s

                                public class Hello;
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

        strSuccess
            Parser.compilationUnit
            "modifiers can be in any order"
            """
            public abstract class MethodModifiers {
                abstract override def everyone(): ();
                override abstract def says() : bool;
                override mutator def hello(): int { }
                mutator override def world(): long { }
                override sealed def but(): (long, int) { return (4L, 5); }
                override sealed mutator def never(): float { 3.141592 }
                override mutator sealed def how(are: int): int { are <- 3; are + 7 }
                mutator override sealed def you(world: MethodModifiers) { world }
            }
            """

        strFailure
            Parser.compilationUnit
            "abstract method cannot have virtual modifier"
            """
            public abstract class Bad {
                abstract virtual def myBadMethod();
            }
            """
            "implies that the method is 'virtual'"

        testStr
            Parser.compilationUnit
            "extern types and members are valid"
            (fun parse ->
                """
                namespace System;

                extern class Random {
                    def new();
                    def new(Seed: int);
                    virtual def Next(maxValue: int): int;
                    virtual def Next(minValue: int, maxValue: int): int;
                }

                extern abstract class Attribute {
                    protected def new();
                }

                extern interface IDisposable {
                    def Dispose(): ();
                }

                extern module Math {
                    def Min(val1: int, val2: int): int;
                }
                """ // TODO: Test class and interface properties.
                |> parse
                |> ParserAssert.isSuccess
                |> fst)
    ]
    |> testList "parser tests"
