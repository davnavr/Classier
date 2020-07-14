﻿namespace Classier.NET.Compiler.SemAnalysis

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Extern
open Classier.NET.Compiler.Grammar.Ast
open Classier.NET.Compiler.Identifier
open Classier.NET.Compiler.IR

type internal Use =
    | UseNamespace of Namespace
    | UseTypeMembers of DefinedOrExtern<GenType, EType>

type internal Usings =
    private
    | Usings of ImmutableSortedDictionary<FullIdentifier<ResolvedType>, ImmutableSortedSet<Use>>

module internal Usings =
    let empty = Usings ImmutableSortedDictionary.Empty

    let private add name gtable (Usings usings) =
        usings

    let ofCompilationUnit (cu: CompilationUnit) =
        invalidOp "bad"
