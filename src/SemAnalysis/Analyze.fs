module Classier.NET.Compiler.SemAnalysis.Analyze

open System.Collections.Immutable
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.IR

type private Analysis<'Result, 'Error> = 'Result * 'Error

let private globals gtable =
    Seq.fold
        (fun (valid, err, table) (acc, tdef) ->
            invalidOp "bad")
        (ImmutableList.Empty, ImmutableList.Empty, gtable)

let output (cunits: seq<CompilationUnit>, epoint: EntryPoint option) gtable: Result<GenOutput, _> =
    invalidOp "no"
