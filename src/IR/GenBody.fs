[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Classier.NET.Compiler.IR.GenBody

open System.Collections.Immutable

let empty ret =
    { Statements = ImmutableList.Empty
      ReturnType = ret }
