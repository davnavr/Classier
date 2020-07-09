[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Classier.NET.Compiler.IR.GenName

open Classier.NET.Compiler
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.Generic

let ofIdentifier id: GenName =
    id
    |> Identifier.mapGenerics
        (fun (TypeParam gen) ->
            { Name = gen.Name
              RequiredInterfaces = List.empty
              RequiredSuperClass = None
              Variance = gen.Variance })
