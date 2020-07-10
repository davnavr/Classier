namespace Classier.NET.Compiler.SemAnalysis

open Classier.NET.Compiler.Grammar

type AnalyzerError =
    | BadExpParam of ExpParam * string
    | DuplicateGlobalType of TypeDef * GlobalTypeSymbol
    | Suppressed of AnalyzerError * reason: AnalyzerError

module AnalyzerError =
    let rec print err =
        invalidOp ""
