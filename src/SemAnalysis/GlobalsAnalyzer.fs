namespace Classier.NET.Compiler.SemAnalysis

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Globals
open Classier.NET.Compiler.ToSource

type GlobalsAnalysis =
    { Duplicates: ImmutableList<Grammar.TypeDef * Grammar.CompilationUnit>
      Table: GlobalsTable
      Valid: ImmutableList<GenType * Grammar.CompilationUnit> }

module GlobalsAnalyzer =
    let analyze table (cunits: seq<Grammar.CompilationUnit>) =
        cunits
        |> Seq.collect
            (fun cunit ->
                Seq.map
                    (fun tdef -> cunit, tdef)
                    cunit.Types)
        |> Seq.fold
            (fun state (cunit, (acc, tdef)) ->
                let gtype =
                    match tdef with
                    | _ -> invalidOp "bad"
                let add =
                    GlobalsTable.addType
                        { Namespace = cunit.Namespace
                          Type = DefinedType (acc, gtype) }
                        state.Table
                match add with
                | Some ntable ->
                    { state with
                        Table = ntable
                        Valid = state.Valid.Add(gtype, cunit) }
                | None ->
                    { state with
                        Duplicates = state.Duplicates.Add(tdef, cunit) })
            { Duplicates = ImmutableList.Empty
              Table = table
              Valid = ImmutableList.Empty }
