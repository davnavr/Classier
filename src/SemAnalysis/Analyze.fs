﻿[<RequireQualifiedAccess>]
module Classier.NET.Compiler.SemAnalysis.Analyze

#nowarn "40"

open System.Collections.Immutable

open Classier.NET.Compiler
open Classier.NET.Compiler.TypeSystem

open Classier.NET.Compiler.Extern
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

/// Adds all global types in the source code and ensures no duplicate names.
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
                GlobalsTable.addType
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
    let ntype smembers gen tmembers update duperr gtype =
        gtype
        |> smembers
        |> Seq.choose
            (function
            | (acc: AccessControl.Access, TypeOrMember.Type nested) ->
                Some(acc, nested)
            | (_, TypeOrMember.Member) -> None)
        |> Seq.fold
            (fun (parent, err) (acc, nested) ->
                let (gnested, nerr) =
                    gen nested parent
                let add =
                    SortedSet.tryAdd
                        (acc, TypeOrMember.Type gnested)
                        (tmembers parent)
                match add with
                | Result.Ok added ->
                    update added parent, ImmList.addRange nerr err
                | Result.Error _ ->
                    parent, ImmList.add (duperr parent nested) err)
            (gtype, ImmutableList.Empty)
    let dupclass ptype parent dup =
        DuplicateClassMember(ptype parent, TypeOrMember.Type dup)
    let dupintf pintf parent dup =
        DuplicateInterfaceMember(pintf parent, TypeOrMember.Type dup)
    let rec nclass =
        ntype
            (fun (clss: GenNestedClass) -> clss.Syntax.Members)
            (fun nested parent ->
                GenType.clss (GenClass.Nested parent) MemberSet.emptyClass nested |> nclass)
            (fun clss -> clss.Members)
            (fun nmembers parent -> { parent with Members = nmembers })
            (dupclass GenClass.Nested)
    let rec ninterface =
        ntype
            (fun (intf: GenNestedInterface) -> intf.Syntax.Members)
            (fun nested parent ->
                GenType.intf (GenInterface.Nested parent) MemberSet.emptyInterface nested |> ninterface)
            (fun intf -> intf.Members)
            (fun nmembers parent -> { parent with Members = nmembers })
            (dupintf GenInterface.Nested)
    anl.GlobalTypes
    |> Seq.indexed
    |> Seq.fold
        (fun state (i, (gtype, cu)) ->
            let nested t tdef create =
                let (gen, err) = create tdef
                { state with
                    Errors = state.Errors.AddRange err
                    GlobalTypes =
                        ImmList.setItem
                            i
                            (t gen, cu)
                            state.GlobalTypes }
            match gtype with
            | GenGlobalClass gclass ->
                ntype
                    (fun (clss: GenGlobalClass) -> clss.Syntax.Members)
                    (fun nested parent ->
                        GenType.clss (GenClass.Global parent) MemberSet.emptyClass nested |> nclass)
                    (fun clss -> clss.Members)
                    (fun nmembers parent -> { parent with Members = nmembers })
                    (dupclass GenClass.Global)
                |> nested
                    GenGlobalClass
                    gclass
            | GenGlobalInterface gintf ->
                ntype
                    (fun (intf: GenGlobalInterface) -> intf.Syntax.Members)
                    (fun nested parent ->
                        GenType.intf (GenInterface.Global parent) MemberSet.emptyInterface nested |> ninterface)
                    (fun intf -> intf.Members)
                    (fun nmembers parent -> { parent with Members = nmembers })
                    (dupintf GenInterface.Global)
                |> nested
                    GenGlobalInterface
                    gintf
            | GenGlobalModule gmdle ->
                ntype
                    (fun (mdle: GenGlobalModule) -> mdle.Syntax.Members)
                    (fun nested parent ->
                        match nested with // TODO: Add the nested types inside of the nested classes, interfaces, and modules of a module.
                        | Class nested ->
                            GenType.clss (GenModule.Global parent) MemberSet.emptyClass nested |> GenNestedClass, Seq.empty
                        | Interface nested ->
                            GenType.intf (GenModule.Global parent) MemberSet.emptyInterface nested |> GenNestedInterface, Seq.empty
                        | Module nested ->
                            GenType.mdle (GenModule.Global parent) MemberSet.emptyModule nested |> GenNestedModule, Seq.empty)
                    (fun mdle -> mdle.Members)
                    (fun nmembers parent -> { parent with Members = nmembers })
                    (fun parent dup ->
                        DuplicateModuleMember(GenModule.Global parent, TypeOrMember.Type dup))
                |> nested
                    GenGlobalModule
                    gmdle)
        anl

let private tresolution cunits anl =
    // NOTE: This checks that the usings are valid.
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
    // TODO: Is there anything else that needs types to be resolved? If not, rename function.

let rec private gexpr expr ltable gtable = // TODO: Clean up this mess of a function.
    match expr with
    | Ast.BoolLit value -> BoolLit value
    | Ast.FuncCall fcall -> // TODO: Make just enough code to make a call to Console.WriteLine work.
        let target = gexpr fcall.Target ltable gtable
        let call t e =
            { Arguments =
                Seq.map
                    (fun arg -> gexpr arg ltable gtable)
                    fcall.Arguments
                |> ImmList.ofSeq
              Target = t }
            |> e
            |> ComplexExpr
        match target with
        | TempEFunctionRef(ns, mdle, func) ->
            call (ns, mdle, func) TempEFunctionCall
        | _ -> failwithf "Unsupported target of function call '%A'" target
    | Ast.IdentifierRef name ->
        match name.Generics with
        | [] when GlobalsTable.hasGlobalNs name.Name gtable ->
            Namespace [ name.Name ] |> NamespaceRef
        | _ -> failwithf "Code to handle identifier '%O' is not yet implemented" name
    | Ast.MemberAccess(target, mber) ->
        let texpr = gexpr target ltable gtable
        match texpr with
        | NamespaceRef ns ->
            let gtype =
                gtable
                |> GlobalsTable.nstypes ns
                |> Seq.tryFind (fun other ->
                    let oname =
                        match other with
                        | Defined gtype -> (GenType.gname gtype).Name
                        | Extern etype -> (EType.gname etype).Name
                    // TODO: Handle any generic parameters or arguments here.
                    oname = mber.Name)
            match gtype with
            | Some gtype -> GlobalTypeRef(ns, gtype)
            | None -> failwithf "The global type '%O' does not exist" mber.Name
        | GlobalTypeRef(ns, gtype) ->
            match gtype with
            | Defined _ -> failwithf "Defined global type %A" gtype
            | Extern etype ->
                match etype with
                | EGlobalModule mdle ->
                    let mfunc = // NOTE: Only functions are considered
                        mdle.Members
                        |> Seq.choose
                            (function
                            | TypeOrMember.Type n -> None
                            | TypeOrMember.Member mdef ->
                                match mdef with
                                | EFunction func -> Some func
                                | _ -> None)
                        |> Seq.tryFind (fun func -> func.FunctionName.Name = mber.Name) // NOTE: Generics aren't even processed yet!
                    match mfunc with
                    | Some mfunc -> TempEFunctionRef(ns, mdle, mfunc)
                    | None ->
                        failwithf "A function with the name '%O' in '%O' could not be found" mber mdle.ModuleName
                | _ -> failwithf "Global extern type %A" etype
        | _ -> failwithf "Code to handle member '%A' of '%A' does not have an implementation" mber target
    | Ast.StrLit str -> StrLit str
    | _ -> failwithf "Unsupported expression '%A'" expr

let private gbody body ltable usings ret gtable =
    let (_, _, result) =
        List.fold
            (fun (table, err, gen) (pos, st) ->
                match st with
                | Ast.IgnoredExpr expr ->
                    let nbody =
                        gexpr expr table gtable
                        |> IgnoredExpr
                        |> GenBody.addTo gen (pos, st)
                    (table, err, nbody)
                | _ -> failwithf "Unsupported statement '%A'" st)
            (ltable, ImmutableList.Empty, GenBody.empty ret)
            body
    // TODO: Don't ignore errors when generating body.
    result

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
            | Result.Ok ltable ->
                let gen =
                    { Parameters =
                        GenParam.ofExpParam
                            args
                            (fun _ -> argtype())
                        |> ImmList.singleton
                      Body = gbody epoint.Body ltable () ImplicitZero anl.GlobalTable
                      Syntax = epoint }
                { anl with Analysis.EntryPoint = Some gen }
            | Result.Error err ->
                Analyzer.error
                    (LocalsTableError err)
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
