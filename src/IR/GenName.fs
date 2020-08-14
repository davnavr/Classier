[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module internal Classier.NET.Compiler.IR.GenName

open Classier.NET.Compiler
open Classier.NET.Compiler.Generic

open Classier.NET.Compiler.Grammar

let ofIdentifier: _ -> GenName =
    Identifier.map
        (fun (TypeParam gen) ->
            { Name = gen.Name
              RequiredInterfaces = List.empty // TODO: Add parameter that is a function that handles resolving the interfaces and superclass.
              RequiredSuperClass = None
              Variance = gen.Variance })
