[<RequireQualifiedAccess>]
module Classier.NET.Compiler.SemAnalysis.Analyze

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.IR

type private Analysis<'Result> =
    { Result: 'Result
      Errors: ImmutableList<AnalyzerError> }

[<RequireQualifiedAccess>]
module private Analysis =
    let toResult a =
        match a.Errors with
        | ImmList.Empty -> Result.Ok a.Result
        | err -> Result.Error err

type private AnalysisBuilder() =
    member _.Bind(a, binder) =
        let part = binder a.Result
        { part with Errors = part.Errors.AddRange a.Errors }
    member _.Return r =
        { Result = r
          Errors = ImmutableList.Empty }

let private analysis = AnalysisBuilder()

let private globals gtable cunits =
    let ctypes cunit =
        Seq.map
            (fun (acc, tdef) ->
                (cunit, acc, tdef))
            cunit.Types
    let (valid, dups, ntable) =
        cunits
        |> Seq.collect ctypes
        |> Seq.fold
            (fun (tlist, elist, table) (cunit, acc, tdef) ->
                let gtype =
                    match tdef with
                    | Class cdef ->
                        cdef
                        |> GenType.gclass MemberSet.emptyClass
                        |> GenClass
                    | Interface intf ->
                        intf
                        |> GenType.ginterface MemberSet.emptyInterface
                        |> GenInterface
                    | Module mdle ->
                        mdle
                        |> GenType.gmodule MemberSet.emptyModule
                        |> GenModule
                let result =
                    GlobalsTable.addSymbol
                        { Namespace = cunit.Namespace
                          Type = Defined (acc, gtype) }
                        table
                match result with
                | Result.Ok ntable ->
                    (ImmList.add (gtype, cunit) tlist, elist, ntable)
                | Result.Error dup ->
                    let err = DuplicateGlobalType (tdef, dup)
                    (tlist, ImmList.add err elist, table))
            (ImmutableList.Empty, ImmutableList.Empty, gtable)
    { Result = valid, ntable
      Errors = dups }

let private ntypes gtypes =
    
    invalidOp "bad"

let output (cunits: seq<CompilationUnit>, epoint: EntryPoint option) table: Result<GenOutput, _> =
    analysis {
        let! (gtypes, gtable) = globals table cunits
        return 0
    }

    let gtypes = globals table cunits
    gtypes
    |> Analysis.toResult
    |> Result.map
        (fun (valid, _) ->
            { GlobalTypes = Seq.map fst valid
              EntryPoint = None })
