module Classier.NET.Compiler.Generic

open Classier.NET.Compiler.Identifier

type GenericVariance =
    | NoVariance
    | Covariant
    | Contravariant

type GenericParam<'Interface, 'SuperClass> =
    { Name: IdentifierStr
      RequiredInterfaces: FullIdentifier<'Interface> list
      RequiredSuperClass: FullIdentifier<'SuperClass> option
      Variance: GenericVariance }

    override this.ToString() = string this.Name

[<StructuralComparison>]
[<StructuralEquality>]
type Generic<'GenericArg, 'Interface, 'SuperClass> =
    | GenericArg of 'GenericArg
    | GenericParam of GenericParam<'Interface, 'SuperClass>

    override this.ToString() =
        match this with
        | GenericArg arg -> arg.ToString()
        | GenericParam gparam -> string gparam

let gparam str =
    { Name = str
      RequiredInterfaces = List.empty
      RequiredSuperClass = None
      Variance = NoVariance }
    |> GenericParam
