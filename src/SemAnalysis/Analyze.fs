namespace Classier.NET.Compiler.SemAnalysis

open Classier.NET.Compiler.Grammar

type AnalyzerError =
    | BadExpParam of ExpParam * string
    | DuplicateType

module Analyze =
    let globals =
        ()

    let output (cunits: seq<CompilationUnit>, epoint: EntryPoint option) gtable =
        //let globals = GlobalsAnalyzer.analyze gtable cunits
        

        invalidOp "no"
