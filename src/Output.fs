﻿namespace Classier.NET.Compiler

open Classier.NET.Compiler.Grammar

type Output =
    { EntryPoint: EntryPoint option }

module Output =
    let write output prnt =
        let block lines =
            seq {
                "{"
                yield! lines
                "#line hidden"
                "}"
            }

        let rec expr e = // TODO: Make this tail-recursive
            match e with
            | FuncCall call ->
                call.Arguments
                |> Seq.map expr
                |> String.concat ", "
                |> sprintf
                    "%s(%s)"
                    (expr call.Target)
            | IdentifierRef id -> string id.Name
            | MemberAccess (e, id) -> sprintf "%s.%s" (expr e) (string id.Name)
            | StrLit str -> sprintf "\"%s\"" str
            | _ ->
                sprintf "#error bad expr /*%A*/" e

        let body (statements: seq<PStatement>) = // TODO: Create a new type for validated statements.
            statements
            |> Seq.map
                (fun (pos, st) ->
                    seq {
                        sprintf // NOTE: These is all quick hacks and low quality code just so I can "Hello World" sooner.
                            "#line %i \"%s\""
                            pos.Line
                            pos.StreamName

                        match st with
                        | IgnoredExpr e -> expr e
                        | _ -> "#error Bad statement"

                        ";"
                    })
            |> Seq.collect id
            |> block

        fun() ->
            seq {
                "// <auto-generated />"
                "#pragma warning disable"
                "#line hidden"

                match output.EntryPoint with
                | Some epoint ->
                    "static class ____Program"
                    yield! block
                        [
                            sprintf
                                "private static void Main(string[] %s)"
                                (epoint.Arguments.Name |> Option.map string |> Option.defaultValue "____args")
                            yield! body epoint.Body
                        ]
                | None -> "// No entry point"

                "#pragma warning restore"
            }
            |> Seq.iter prnt
