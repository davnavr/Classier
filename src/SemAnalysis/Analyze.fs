[<RequireQualifiedAccess>]
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
      GlobalTypes: ImmutableList<GenType * CompilationUnit * Usings> }

module private Analyzer =
    let init table =
        { EntryPoint = None
          Errors = ImmutableList.Empty
          GlobalTable = table
          GlobalTypes = ImmutableList.Empty }

    let error err anl =
        { anl with Errors = anl.Errors.Add err }

let private globals cunits anl =
    let ctypes (cunit: CompilationUnit) =
        Seq.map
            (fun (acc, tdef) ->
                (cunit, acc, tdef))
            cunit.Types
    cunits
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
                    GlobalTypes = state.GlobalTypes.Add (gtype, cunit, Usings.empty) }
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
        (fun state (i, (gtype, cu, us)) ->
            let addNested create t tdef =
                let (gen, err) = create tdef
                { state with
                    Errors = state.Errors.AddRange err
                    GlobalTypes =
                        ImmList.setItem
                            i
                            (t gen, cu, us)
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

let private tresolution anl =
    anl.GlobalTypes
    |> Seq.indexed
    |> Seq.fold
        (fun state _ ->
            invalidOp "TODO: Resolve all of the usings")
        anl

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
        |> tresolution
    let result =
        rtypes
        // |> members
        |> entryPoint epoint
    match result.Errors with
    | ImmList.Empty ->
        { GlobalTypes =
            Seq.map
                (fun (tdef, cu, _) -> cu.Namespace, tdef)
                result.GlobalTypes
          EntryPoint = result.EntryPoint }
        |> Result.Ok
    | err -> Result.Error err
