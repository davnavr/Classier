namespace Classier.NET.Compiler.SemAnalysis

open System.Collections.Immutable

open Classier.NET.Compiler

open Classier.NET.Compiler.Grammar.Ast
open Classier.NET.Compiler.IR

[<RequireQualifiedAccess>]
module Usings =
    type private Use =
        | UseNamespace of Namespace
        | UseTypeMembers of GenType
    
    type Usings =
        private
        | Usings of ImmutableSortedDictionary<FullIdentifier<TypeName>, ImmutableSortedSet<Use>>

    let empty = Usings ImmutableSortedDictionary.Empty

    let private add (pos, uname) gtable (Usings usings) =
        match uname with
        | FullIdentifier.Namespace strs ->
            result {
                let! uset =
                    ImmSortedDict.tryGetValue
                        uname
                        (fun uset ->
                            SortedSet.tryAdd
                                (Namespace strs |> UseNamespace)
                                uset
                            |> Result.mapError (fun _ -> pos, uname))
                        (fun() ->
                            SortedSet.add
                                (Namespace strs |> UseNamespace)
                                ImmutableSortedSet.Empty
                            |> Result.Ok)
                        usings
                let result =
                    ImmSortedDict.setItem
                        uname
                        uset
                        usings
                    |> Usings
                return result
            }
        | _ -> invalidOp "Using statements are only currently supported for namespaces"

    let ofCompilationUnit gtable (cu: CompilationUnit) =
        cu.Usings
        |> List.toSeq
        |> Seq.fold
            (fun (usings, err) name ->
                match add name gtable usings with
                | Result.Ok added ->
                    added, err
                | Result.Error e ->
                    usings, ImmList.add e err)
            (empty, ImmutableList.Empty)

type Usings = Usings.Usings
