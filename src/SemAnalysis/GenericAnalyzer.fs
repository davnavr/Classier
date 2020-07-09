module Classier.NET.Compiler.GenericAnalyzer

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.Identifier
open Classier.NET.Compiler.IR
open Classier.NET.Compiler.ToSource

let validateName (name: Grammar.Ast.GenericName) =
    let gname = GenName.ofIdentifier name.Identifier
    match name.Identifier.Generics with
    | [] -> Result.Ok gname
    | generics ->
        Seq.fold
            (fun (mem, res) (Grammar.Ast.TypeParam gparam) ->
                match res with
                | Result.Ok (buildp: ImmutableList<_>) ->
                    match SortedSet.tryAdd gparam.Name mem with
                    | None ->
                        mem, Result.Error gparam.Name
                    | Some nodup ->
                        let nres =
                            { Name = gparam.Name
                              RequiredInterfaces = List.empty
                              RequiredSuperClass = None
                              Variance = gparam.Variance }
                            |> buildp.Add
                            |> Result.Ok
                        nodup, nres
                | Result.Error _ -> mem, res)
            (ImmutableSortedSet.Empty, Result.Ok ImmutableList.Empty)
            generics
        |> snd
        |> Result.map
            (fun valid -> { gname with Generics = List.ofSeq valid })

// TODO: Create function to resolve the RequiredInterfaces and RequiredSuperClass.
