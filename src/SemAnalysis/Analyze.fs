﻿[<RequireQualifiedAccess>]
module Classier.NET.Compiler.SemAnalysis.Analyze

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.IR
open Classier.NET.Compiler.TypeSystem

type private Analysis<'Data> =
    { Data: 'Data 
      EntryPoint: GenEntryPoint option
      Errors: ImmutableList<AnalyzerError>
      GlobalTable: GlobalsTable
      GlobalTypes: ImmutableList<GenType * CompilationUnit> }

module private Analyzer =
    let init table =
        { Data = ()
          EntryPoint = None
          Errors = ImmutableList.Empty
          GlobalTable = table
          GlobalTypes = ImmutableList.Empty }

    let setData data anl =
        // Copy expression is not used in order to allow different types for 'Data
        { Data = data
          EntryPoint = anl.EntryPoint
          Errors = anl.Errors
          GlobalTable = anl.GlobalTable
          GlobalTypes = anl.GlobalTypes }

    let error err anl =
        { anl with Errors = anl.Errors.Add err }
    let check checker success failure anl: Analysis<_> =
        match checker anl with
        | Result.Ok data ->
            setData data anl
            |> success
        | Result.Error err ->
            anl
            |> error err
            |> failure

let private globals anl =
    let ctypes (cunit: CompilationUnit) =
        Seq.map
            (fun (acc, tdef) ->
                (cunit, acc, tdef))
            cunit.Types
    anl.Data
    |> Seq.collect ctypes
    |> Seq.fold
        (fun state (cunit, acc, tdef) ->
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
    let tclass = GenType.gclass MemberSet.emptyClass
    let tinterface = GenType.ginterface MemberSet.emptyInterface

    let nested (syntaxm: _ -> MemberList<_, _>) gtype =
        gtype
        |> syntaxm
        |> Seq.choose
            (function
            | (acc: AccessControl.Access, TypeOrMember.Type nested) ->
                Some(acc, nested)
            | (_, TypeOrMember.Member _) -> None)
    let rec ntype syntaxm ctype typem update duperr gtype =
        let inner = ntype syntaxm ctype typem update duperr
        gtype
        |> nested syntaxm
        |> Seq.fold
            (fun (parent, err) (acc, nested) ->
                let (gnested, nerr) =
                    ctype nested |> inner
                let add =
                    SortedSet.tryAdd
                        (acc, TypeOrMember.Type gnested)
                        (typem parent)
                match add with
                | Some added ->
                    let errors = ImmList.addRange nerr err
                    update added parent, errors
                | None ->
                    let dup = duperr(parent, TypeOrMember.Type nested)
                    parent, ImmList.add dup err)
            (gtype, ImmutableList.Empty)

    let nclass =
        ntype
            (fun (clss: GenClass) -> clss.Syntax.Members)
            tclass
            (fun clss -> clss.Members)
            (fun added parent -> { parent with Members = added })
            DuplicateClassMember
    let ninterface =
        ntype
            (fun (intf: GenInterface) -> intf.Syntax.Members)
            tinterface
            (fun intf -> intf.Members)
            (fun added parent -> { parent with Members = added })
            DuplicateInterfaceMember
    let rec nmodule (mdle: GenModule) =
        nested
            (fun _ -> mdle.Syntax.Members)
            mdle
        |> Seq.fold
            (fun (parent, err) (acc, nested) ->
                let (gnested, nerr) =
                    let atype (tmap: _ -> GenType) (ntype, err) =
                        tmap ntype, err
                    match nested with
                    | Class clss ->
                        tclass clss
                        |> nclass
                        |> atype GenClass
                    | Interface intf ->
                        tinterface intf
                        |> ninterface
                        |> atype GenInterface
                    | Module nmdle ->
                        GenType.gmodule
                            MemberSet.emptyModule
                            nmdle
                        |> nmodule
                        |> atype GenModule
                let add =
                    SortedSet.tryAdd
                        (acc, TypeOrMember.Type gnested)
                        parent.Members
                match add with
                | Some added ->
                    { parent with Members = added }, err.AddRange nerr
                | None ->
                    let dup = DuplicateModuleMember(parent, TypeOrMember.Type nested)
                    parent, err.Add dup)
            (mdle, ImmutableList.Empty)

    anl.GlobalTypes
    |> Seq.indexed
    |> Seq.fold
        (fun state (i, (gtype, cu)) ->
            let addNested create t tdef =
                let (gen, err) = create tdef
                { state with
                    Errors = state.Errors.AddRange err
                    GlobalTypes =
                        ImmList.setItem
                            i
                            (t gen, cu)
                            state.GlobalTypes }
            match gtype with
            | GenClass clss ->
                addNested
                    nclass
                    GenClass
                    clss
            | GenInterface intf ->
                addNested
                    ninterface
                    GenInterface
                    intf
            | GenModule mdle ->
                addNested
                    nmodule
                    GenModule
                    mdle)
        anl

let private gbody anl =
    let result =
        List.fold
            (fun state (pos, st) ->
                match state with
                | Result.Ok (ltable, body) ->
                    match st with
                    | _ ->
                        sprintf "analyzer for statement %A" st
                        |> FeatureNotImplemented
                        |> Result.Error
                | Result.Error _ -> state)
            (Result.Ok (snd anl.Data, GenBody.empty))
            (fst anl.Data)
    anl

let private members anl =
    invalidOp "bad"

let private entryPoint =
    let argtype() = 
        PrimitiveType.String
        |> Primitive
        |> ArrayType
    Analyzer.check
        (fun (anl: Analysis<EntryPoint>) ->
            match (anl.Data.Parameters) with
            | [ args ] when args.Type = (argtype() |> TypeName) ->
                LocalsTable.empty
                |> LocalsTable.enterScope
                |> LocalsTable.addExpParam
                    args
                    (fun _ -> argtype())
                |> Option.map
                    (fun ltable -> Result.Ok (anl.Data, ltable))
                |> Option.defaultValue
                    (InternalAnalyzerError "Unable to add the parameter to the locals table" |> Result.Error)
            | _ ->
                BadEntryPointSignature anl.Data |> Result.Error)
        (fun anl ->
            invalidOp "")
        (Analyzer.setData ())

let output (cunits, epoint) table =
    let result =
        Analyzer.init table
        |> Analyzer.setData cunits
        |> globals
        |> ntypes
        // |> resolveTheUsingsOfTheCuOrSomething
        // |> members
        |> Analyzer.setData epoint
        |> entryPoint
    match result.Errors with
    | ImmList.Empty ->
        { GlobalTypes =
            Seq.map
                (fun (tdef, cu) -> cu.Namespace, tdef)
                result.GlobalTypes
          EntryPoint = result.EntryPoint }
        |> Result.Ok
    | err -> Result.Error err
