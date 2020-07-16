namespace Classier.NET.Compiler.SemAnalysis

open System.Collections.Immutable

open Classier.NET.Compiler
open Classier.NET.Compiler.Identifier

open Classier.NET.Compiler.Extern
open Classier.NET.Compiler.Grammar.Ast
open Classier.NET.Compiler.IR

[<RequireQualifiedAccess>]
module Usings =
    type private Use =
        | UseNamespace of Namespace
        | UseTypeMembers of DefinedOrExtern<GenType, EType>
    
    type Usings =
        private
        | Usings of ImmutableSortedDictionary<FullIdentifier<ResolvedType>, ImmutableSortedSet<Use>>

    let empty = Usings ImmutableSortedDictionary.Empty

    let private add (pos, uname) gtable (Usings usings) =
        match Identifier.fullAsList uname with
        | [ name ] ->
            invalidOp "add the thing and see if it is valid"

    let ofCompilationUnit gtable (cu: CompilationUnit) =
        Seq.fold
            (fun (usings, err) name ->
                match add name gtable usings with
                | Result.Ok added ->
                    added, err
                | Result.Error e ->
                    usings, ImmList.addRange e err)
            (empty, ImmutableList.Empty)
            cu.Usings

type Usings = Usings.Usings
