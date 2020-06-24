module Classier.NET.Compiler.Analyzer

open System.Collections.Immutable
open Classier.NET.Compiler.Grammar

type AnalyzerError =
    | BadExpParam of ExpParam * string
    | DuplicateType

let analyzeOutput (cunits: seq<CompilationUnit>, epoint: EntryPoint) gtable =
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
