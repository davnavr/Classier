namespace Classier.NET.Compiler.SemAnalysis

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Globals
open Classier.NET.Compiler.ToSource

type GlobalsAnalysis<'Errors> =
    { Errors: 'Errors
      Table: GlobalsTable
      Valid: ImmutableList<GenType * Grammar.CompilationUnit> }

module GlobalsAnalyzer =
    let analyze table (cunits: seq<Grammar.CompilationUnit>) = // TODO: Rename this function to something like "addGlobalTypesToTable".
        cunits
        |> Seq.collect
            (fun cunit ->
                Seq.map
                    (fun tdef -> cunit, tdef)
                    cunit.Types)
        |> Seq.fold
            (fun state (cunit, (acc, tdef)) ->
                let gtype =
                    match tdef with
                    | Grammar.Class clss ->
                        { ClassName = clss.ClassName.Identifier
                          Interfaces = ImmutableSortedSet.Empty
                          Members = ImmutableSortedSet.Empty // TODO: Create empty variants for GenClass members set and the members set of other types.
                          SuperClass = None
                          Syntax = clss }
                        |> GenClass
                    | Grammar.Interface intf ->
                        { InterfaceName = intf.InterfaceName.Identifier
                          Members = ImmutableSortedSet.Empty
                          SuperInterfaces = ImmutableSortedSet.Empty
                          Syntax = intf }
                        |> GenInterface
                    | Grammar.Module modl ->
                        { Members = ImmutableSortedSet.Empty
                          ModuleName = modl.ModuleName.Identifier
                          Syntax = modl }
                        |> GenModule
                let add =
                    GlobalsTable.addType
                        { Namespace = cunit.Namespace
                          Type = DefinedType (acc, gtype) }
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

    // NOTE: You can get a TypeDef from a GenType by using the Syntax property.
    let resolveSuperClass table gtypes =
        invalidOp "no impl to find super classes"
