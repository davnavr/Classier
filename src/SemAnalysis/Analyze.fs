[<RequireQualifiedAccess>]
module Classier.NET.Compiler.SemAnalysis.Analyze

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.IR

type private Analysis<'Result, 'Error> =
    | Success of 'Result
    | WithError of 'Result * 'Error

[<RequireQualifiedAccess>]
module private Analysis =
    let toResult =
        function
        | Success success -> Result.Ok success
        | WithError(_, err) -> Result.Error err

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
            (fun (tlist, err, table) (cunit, acc, tdef) ->
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
                    (ImmList.add (gtype, cunit) tlist, err, ntable)
                | Result.Error dup ->
                    (tlist, ImmList.add dup err, table))
            (ImmutableList.Empty, ImmutableList.Empty, gtable)
    let result = valid, ntable
    match dups with
    | ImmList.Empty -> Success result
    | _ ->
        WithError(result, dups)

let output (cunits: seq<CompilationUnit>, epoint: EntryPoint option) gtable: Result<GenOutput, _> =
    let gtypes = globals gtable cunits
    gtypes
    |> Analysis.toResult
    |> Result.map
        (fun (valid, _) ->
            { GlobalTypes = Seq.map fst valid
              EntryPoint = None })
