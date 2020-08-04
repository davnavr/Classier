[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Classier.NET.Compiler.IR.GenName

open Classier.NET.Compiler
open Classier.NET.Compiler.Generic

open Classier.NET.Compiler.Extern
open Classier.NET.Compiler.Grammar

let ofIdentifier id: GenName =
    id
    |> Identifier.mapGenerics
        (fun (TypeParam gen) ->
            { Name = gen.Name
              RequiredInterfaces = List.empty // TODO: Add parameter that is a function that handles resolving the interfaces and superclass.
              RequiredSuperClass = None
              Variance = gen.Variance })
