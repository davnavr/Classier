﻿namespace Classier.NET.Compiler.SemAnalysis

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.ToSource

type GlobalsAnalysis<'Errors> =
    { Errors: 'Errors
      Table: GlobalsTable
      Valid: ImmutableList<GenType * Grammar.CompilationUnit> }

module GlobalsAnalyzer =
    let init table =
        Seq.fold
            (fun state (cunit: Grammar.CompilationUnit, acc, tdef) ->
                let gtype =
                    match tdef with
                    | Grammar.Class clss ->
                        GenType.gclass
                            ImmutableSortedSet.Empty
                            MemberSet.classSet
                            clss
                        |> GenClass
                    | Grammar.Interface intf ->
                        { InterfaceName =
                            intf.InterfaceName.Identifier
                            |> GenName.ofIdentifier
                          Members = MemberSet.interfaceSet
                          SuperInterfaces = ImmutableSortedSet.Empty
                          Syntax = intf }
                        |> GenInterface
                    | Grammar.Module modl ->
                        { Members = MemberSet.moduleSet
                          ModuleName = modl.ModuleName.Identifier
                          Syntax = modl }
                        |> GenModule
                let add =
                    GlobalsTable.addSymbol
                        { Namespace = cunit.Namespace
                          Type = DefinedGlobal (acc, gtype) }
                        state.Table
                match add with
                | Some ntable ->
                    { state with
                        Table = ntable
                        Valid = state.Valid.Add(gtype, cunit) }
                | None ->
                    let errors: ImmutableList<Grammar.TypeDef * Grammar.CompilationUnit> = state.Errors
                    { state with Errors = errors.Add(tdef, cunit) })
            { Errors = ImmutableList.Empty
              Table = table
              Valid = ImmutableList.Empty }

    (*
    Steps should be:
    
    1. Add all global types to symbol table
    NOTE: There should be a function to replace a GenType in the GlobalsTable.
    2. Temporarily process all members to allow resolution of nested types in the next 2 steps
    3. Validate SuperClasses for all classes
    4. Validate Interfaces for all classes and interfaces
    5. Check generic parameters of all members and types to ensure no duplicates and validation of required interfaces or superclass
    
    After that, bodies of methods, constructors, etc. should be processed by another module or function, with the validated bodies of the member replacing the temporary version created in step 2
    *)

    // NOTE: You can get a TypeDef from a GenType by using the Syntax property.
    let resolveSuperClass table gtypes =
        invalidOp "no impl to find super classes"
