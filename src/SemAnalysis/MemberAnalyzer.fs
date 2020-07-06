namespace Classier.NET.Compiler.SemAnalysis

open Classier.NET.Compiler
open Classier.NET.Compiler.ToSource

module MemberAnalyzer =
    let init: (GlobalsTable -> seq<Grammar.CompilationUnit * AccessControl.Access * _> -> GlobalsTable) =
        let rec gmember table (cunit, acc, tdef) =
            MemberSet.fromSyntax tdef
            invalidOp "bad"
        Seq.fold
            (fun table gtype ->
                gmember table gtype)
