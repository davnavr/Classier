namespace Classier.NET.Compiler.SemAnalysis

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.ToSource

type GlobalsAnalysis =
    { Duplicates: ImmutableList<Grammar.TypeDef * Grammar.CompilationUnit>
      Table: GlobalsTable
      Valid: ImmutableList<Grammar.CompilationUnit> }

module GlobalsAnalyzer =
    let analyze cunits table =
        Seq.fold
            (fun _ cunit ->
                invalidOp "bad")
            { Duplicates = ImmutableList.Empty
              Table = table
              Valid = ImmutableList.Empty }
            cunits
