module Classier.NET.Compiler.SemAnalysis.AnalyzerError

open Classier.NET.Compiler.Grammar

type AnalyzerError =
    | BadExpParam of ExpParam * string
    | DuplicateType
    | Suppressed of AnalyzerError * reason: AnalyzerError
