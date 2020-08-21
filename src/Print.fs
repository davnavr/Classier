﻿[<RequireQualifiedAccess>]
module Classier.NET.Compiler.Print

open System

open Classier.NET.Compiler.IR

type Print = Print of ((string -> unit) -> unit)

let nothing = Print ignore
let it out (Print p) = p out

let many plist =
    Print (fun prnt ->
        Seq.iter
            (it prnt)
            plist)

type PrintBuilder() =
    member _.Combine(Print p1, Print p2) =
        Print (fun out -> p1 out; p2 out)
    member _.Delay f =
        Print (fun out -> f() |> it out)
    member _.For(items, f) =
        Seq.map f items |> many
    member _.Yield (p: Print) = p
    member _.Zero() = nothing

let print = PrintBuilder()

let str s = Print (fun prnt -> prnt s)
let strf fmt = Core.Printf.ksprintf str fmt

let line p = many [ p; str "\n" ]

let strl s = str s |> line

let indented by p = many [ String(' ', by) |> str; p ]

let block indent p =
    print {
        strl "{"
        indented indent p
        strl "}"
    }
let paren p = many [ str "("; p; str ")" ]

let pposition (pos: FParsec.Position) =
    strf
        "#line %i \"%s\""
        pos.Line
        pos.StreamName
    |> line

let rec pexpr (expr: GenExpression) = // TODO: FInd a better way to make this recursive
    match expr with
    | BoolLit b -> strf "%b" b
    | StrLit s ->
        print {
            str "\""
            str s // TODO: How to handle special characters and escape sequences? Are they replaced by the parser?
            str "\""
        }
    | _ -> strl "#error Bad expression"

let pbody (body: GenBody) indent =
    print {
        for (st, (pos, syntax)) in body do
            pposition pos
            match st with
            | Empty -> strl ";"
            | IgnoredExpr expr ->
                pexpr expr // TODO: Does computation expression cause this to be skipped or will it work correctly?
                strl ";" |> line
            | _ -> strl "#error Bad statement"
    }
    |> block indent

let pepoint output indent =
    match output.EntryPoint with
    | Some epoint ->
        print {
            strl "internal static class _EntryPoint"

            print {
                strl "private static void Main()"
                pbody epoint.Body indent
            }
            |> block indent
        }
    | None -> strl "// No entry point"

let poutput (output: GenOutput) indent = // TODO: How will namespaces be included?
    print {
        strl "// <auto-generated />"
        strl "#pragma warning disable"
        pepoint output indent
    }
