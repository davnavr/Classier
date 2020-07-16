[<RequireQualifiedAccess>]
module Classier.NET.Compiler.SemAnalysis.Analyze

open System.Collections.Immutable

open Classier.NET.Compiler
open Classier.NET.Compiler.TypeSystem

open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.IR

type private Analysis =
    { EntryPoint: GenEntryPoint option
      Errors: ImmutableList<AnalyzerError>
      GlobalTable: GlobalsTable
      GlobalTypes: ImmutableList<GenGlobalType * CompilationUnit>
      Usings: ImmutableSortedDictionary<CompilationUnit, Usings> }

module private Analyzer =
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
          GlobalTypes = ImmutableList.Empty
          Usings = usings }

    let error err anl =
        { anl with Errors = anl.Errors.Add err }

let private globals cunits anl =
    let ctypes (cunit: CompilationUnit) =
        Seq.map
            (fun (_, tdef) -> cunit, tdef)
            cunit.Types
    cunits
    |> Seq.collect ctypes
    |> Seq.fold
        (fun state (cunit, tdef) ->
            let tcreate fsyntax mset gtype def =
                def
                |> fsyntax cunit.Namespace mset
                |> gtype
            let gtype =
                match tdef with
                | Class cdef ->
                    tcreate
                        GenType.clss
                        MemberSet.emptyClass
                        GenGlobalClass
                        cdef
                | Interface intf ->
                    tcreate
                        GenType.intf
                        MemberSet.emptyInterface
                        GenGlobalInterface
                        intf
                | Module mdle ->
                    tcreate
                        GenType.mdle
                        MemberSet.emptyModule
                        GenGlobalModule
                        mdle
            let result =
                GlobalsTable.addSymbol
                    (Defined gtype)
                    cunit.Namespace
                    state.GlobalTable
            match result with
            | Result.Ok ntable ->
                { state with
                    GlobalTable = ntable
                    GlobalTypes = state.GlobalTypes.Add (gtype, cunit) }
            | Result.Error dup ->
                Analyzer.error
                    (DuplicateGlobalType (tdef, dup))
                    state)
        anl

let private ntypes anl =
    anl

let private tresolution cunits anl =
    Seq.fold
        (fun state cu ->
            let (uses, err) =
                Usings.ofCompilationUnit
                    anl.GlobalTable
                    cu
            { state with
                Usings =
                    state.Usings.SetItem(cu, uses)
                Errors =
                    err
                    |> Seq.map BadUseStatement
                    |> state.Errors.AddRange })
        anl
        cunits

let private gbody body ltable anl =
    anl

let private members anl =
    invalidOp "bad"

let private entryPoint (epoint: EntryPoint option) anl =
    let argtype() = 
        PrimitiveType.String
        |> Primitive
        |> ArrayType
    match epoint with
    | Some epoint ->
        match epoint.Parameters with
        | [ args ] when args.Type = (argtype() |> TypeName) ->
            let ltable =
                LocalsTable.empty
                |> LocalsTable.enterScope
                |> LocalsTable.addExpParam
                    args
                    (fun _ -> argtype())
            match ltable with
            | Some ltable ->
                let gen =
                    invalidOp "TODO: create the entry point"
                { anl with Analysis.EntryPoint = Some gen }
            | None ->
                Analyzer.error
                    (InternalAnalyzerError "Unable to add the parameter to the locals table")
                    anl
        | _ ->
            Analyzer.error
                (BadEntryPointSignature epoint)
                anl
    | None -> anl

let output (cunits, epoint) table =
    let rtypes =
        Analyzer.init table
        |> globals cunits
        |> ntypes
        |> tresolution cunits
    let result =
        rtypes
        // |> members
        |> entryPoint epoint
    match result.Errors with
    | ImmList.Empty ->
        { GlobalTypes =
            Seq.map fst result.GlobalTypes
          EntryPoint = result.EntryPoint }
        |> Result.Ok
    | err -> Result.Error err
