﻿[<RequireQualifiedAccess>]
module Classier.NET.Compiler.SemAnalysis.Analyze

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.IR
open Classier.NET.Compiler.TypeSystem

type private Analysis =
    { EntryPoint: GenEntryPoint option
      Errors: ImmutableList<AnalyzerError>
      GlobalTable: GlobalsTable
      GlobalTypes: ImmutableList<GenType * CompilationUnit> }

module private Analyzer =
    let error anl err =
        { anl with Errors = anl.Errors.Add err }
    let check checker anl =
        match checker anl with
        | Some err -> error anl err
        | None -> anl
    let next (f: Analysis -> Analysis) anl =
        f anl

// TODO: Maybe pass in an empty 'Analysis' and create and use new functions in the Analyzer module to chain together the work?
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

let private gbody (body: PStatement list) ltable anl = // TODO: Maybe make Analysis generic so it can pass data around to different analyzer functions?
    List.fold
        (fun (state, locals) (pos, st) ->
            match st with
            | _ ->
                let err =
                    st
                    |> sprintf "analyzer for statement %A"
                    |> FeatureNotImplemented
                    |> Analyzer.error state
                err, locals)
        (anl, ltable)
        body
    |> fst

let private members anl =
    invalidOp "bad"

let private entryPoint epoint =
    match epoint with
    | Some (epoint: EntryPoint) ->
        Analyzer.check
            (fun _ ->
                let err = BadEntryPointSignature epoint |> Some
                match (epoint.Parameters) with // TODO: Come up with way to make this chain of matches shorter.
                | [ args ] ->
                    let (TypeName ptype) = args.Type
                    match ptype with
                    | ArrayType oftype ->
                        match oftype with
                        | Primitive prim ->
                            match prim with
                            | PrimitiveType.String -> None
                            | _ -> err
                        | _ -> err
                    | _ -> err
                | _ -> err)
        >> Analyzer.next
            (fun anl ->
                let body =
                    LocalsTable.empty
                    // |> LocalsTable.addLocal
                    // |> gbody 
                invalidOp "TODO: Validate the body of the entrypoint")
    | None -> id

let output (cunits, epoint) table =
    let result =
        globals table cunits
        |> ntypes
        // |> members
        |> entryPoint epoint
    match result.Errors with
    | ImmList.Empty ->
        { GlobalTypes =
            Seq.map
                (fun (tdef, cu) -> cu.Namespace, tdef)
                result.GlobalTypes
          EntryPoint = result.EntryPoint }
        |> Result.Ok
    | err -> Result.Error err
