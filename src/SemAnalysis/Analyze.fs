[<RequireQualifiedAccess>]
module Classier.NET.Compiler.SemAnalysis.Analyze

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.IR

type private Analysis =
    { EntryPoint: GenEntryPoint option
      Errors: ImmutableList<AnalyzerError>
      GlobalTable: GlobalsTable
      GlobalTypes: ImmutableList<GenType * CompilationUnit> }

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
    { EntryPoint = None
      Errors = dups
      GlobalTable = ntable
      GlobalTypes = valid }

let private ntypes acc =
    Seq.fold
        (fun state (gtype, cu) ->
            match gtype with
            | GenClass clss ->
                invalidOp "bad class"
            | GenInterface intf ->
                invalidOp "bad interface"
            | GenModule mdle ->
                invalidOp "bad module")
        acc
        acc.GlobalTypes
    // TODO: Somehow replace the generated types in the GlobalTable with GlobalTypes.

let output (cunits, epoint: EntryPoint option) table =
    let result =
        globals table cunits
        |> ntypes
    match result.Errors with
    | ImmList.Empty ->
        { GlobalTypes =
            Seq.map fst result.GlobalTypes
          EntryPoint = result.EntryPoint }
        |> Result.Ok
    | err -> Result.Error err
