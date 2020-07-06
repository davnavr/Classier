module internal Classier.NET.Compiler.ToSource.GenName

open Classier.NET.Compiler
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.Generic

let ofIdentifier id: GenName =
    id
    |> TypeParam.identifierMap
    |> Identifier.mapGenerics
        (fun (gen: GenericParam<_, _>) ->
            { Name = gen.Name
              RequiredInterfaces = List.empty
              RequiredSuperClass = None
              Variance = gen.Variance })
