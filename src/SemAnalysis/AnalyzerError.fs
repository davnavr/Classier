namespace Classier.NET.Compiler.SemAnalysis

open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.IR

type AnalyzerError =
    | BadExpParam of ExpParam * string
    | DuplicateGlobalType of TypeDef * GlobalTypeSymbol
    | DuplicateClassMember of GenClass // * TypeOrMember<GenClass, GenClassMember>
    | Suppressed of AnalyzerError * reason: AnalyzerError

module AnalyzerError =
    let rec print err =
        invalidOp ""
