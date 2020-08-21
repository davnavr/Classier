namespace Classier.NET.Compiler.SemAnalysis

open System.Collections.Immutable

open Classier.NET.Compiler

open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.IR

type Analysis =
    { EntryPoint: GenEntryPoint option
      Errors: ImmutableList<AnalyzerError>
      GlobalDecls: ImmutableList<GenGlobalDecl * CompilationUnit>
      GlobalTable: Globals.Table
      Usings: ImmutableSortedDictionary<CompilationUnit, Usings> }

[<AutoOpen>]
module Analysis =
    type AnalysisBuilder internal () =
        member _.ReturnFrom(anl: Analysis) = anl

    let analyze = AnalysisBuilder()

    let init table =
        let usings =
            ImmSortedDict.withKeyComparer
                (fun cu1 cu2 ->
                    compare
                        cu1.Source
                        cu2.Source)
                ImmutableSortedDictionary.Empty
        { EntryPoint = None
          Errors = ImmutableList.Empty
          GlobalTable = table
          GlobalDecls = ImmutableList.Empty
          Usings = usings }
