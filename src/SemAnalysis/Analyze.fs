namespace Classier.NET.Compiler.SemAnalysis

open System.Collections.Immutable
open Classier.NET.Compiler.Grammar

type AnalyzerError =
    | BadExpParam of ExpParam * string
    | DuplicateType

module Analyze =
    let output (cunits: seq<CompilationUnit>, epoint: EntryPoint) gtable = // TODO: Create separate record types and modules to handle analysis of different things.
        let analyzeTypes cunit gtable =
            Seq.fold
                (fun _ _ -> invalidOp "bad")
                (ImmutableList.Empty, gtable)
                cunit.Types
        let analyzeBlock ltable =
            invalidOp "bad"
        let analyzeEntryPoint (epoint: EntryPoint) =
            invalidOp "nope"

        invalidOp "no"
