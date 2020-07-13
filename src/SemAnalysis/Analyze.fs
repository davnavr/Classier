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

let private ntypes anl =
    let ntype1 =
        function
        | (acc: AccessControl.Access, TypeOrMember.Type nested) ->
            Some(acc, nested)
        | (_, TypeOrMember.Member _) -> None
    let rec ntype syntaxm ctype typem update duperr gtype =
        let inner = ntype syntaxm ctype typem update duperr
        gtype
        |> syntaxm
        |> Seq.choose
            (function
            | (acc: AccessControl.Access, TypeOrMember.Type nested) ->
                Some(acc, nested)
            | (_, TypeOrMember.Member _) -> None)
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
    let rec nclass (clss: GenClass) =
        clss.Syntax.Members
        |> Seq.choose ntype1
        |> Seq.fold
            (fun (parent: GenClass, err) (acc, nested) ->
                let (gnested, nerr) =
                    GenType.gclass
                        MemberSet.emptyClass
                        nested
                    |> nclass
                let add =
                    SortedSet.tryAdd
                        (acc, TypeOrMember.Type gnested)
                        parent.Members
                match add with
                | Some added ->
                    let errors = ImmList.addRange nerr err
                    { parent with Members = added }, errors
                | None ->
                    let dup = DuplicateClassMember(parent, TypeOrMember.Type nested)
                    parent, ImmList.add dup err)
            (clss, ImmutableList.Empty)
    let rec ninterface =
        ntype
            (fun (idef: GenInterface) -> idef.Syntax.Members)
            (GenType.ginterface MemberSet.emptyInterface)
            (fun intf -> intf.Members)
            (fun added parent -> { parent with Members = added })
            DuplicateInterfaceMember
    anl.GlobalTypes
    |> Seq.indexed
    |> Seq.fold
        (fun state (i, (gtype, cu)) ->
            match gtype with
            | GenClass clss ->
                let (gen, err) = nclass clss // TODO: Create function for this block.
                { state with
                    Errors = state.Errors.AddRange err
                    GlobalTypes =
                        ImmList.setItem
                            i
                            (GenClass gen, cu)
                            state.GlobalTypes }
            | GenInterface intf ->
                let (gen, err) = ninterface intf
                { state with
                    Errors = state.Errors.AddRange err
                    GlobalTypes =
                        ImmList.setItem
                            i
                            (GenInterface gen, cu)
                            state.GlobalTypes }
            | GenModule mdle ->
                invalidOp "bad module")
        anl
    // TODO: Somehow replace the generated types in the GlobalTable with GlobalTypes.

let output (cunits, epoint: EntryPoint option) table =
    let result =
        globals table cunits
        |> ntypes
    match result.Errors with
    | ImmList.Empty ->
        { GlobalTypes =
            Seq.map fst result.GlobalTypes // TODO: Should instead return a sequence of GenType with Namespace.
          EntryPoint = result.EntryPoint }
        |> Result.Ok
    | err -> Result.Error err
