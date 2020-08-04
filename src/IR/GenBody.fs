[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Classier.NET.Compiler.IR.GenBody

open System.Collections.Immutable

open Classier.NET.Compiler

let empty ret =
    { Statements = ImmutableList.Empty
      ReturnType = ret }

let addTo body syntax statement =
    { body with
        Statements = ImmList.add (statement, syntax) body.Statements }
