module Classier.NET.Compiler.Generic

open Classier.NET.Compiler.Identifier

type GenericVariance =
    | NoVariance
    | Covariant
    | Contravariant

type GenericParam<'Interface, 'SuperClass> =
    { Name: IdentifierStr
      RequiredInterfaces: 'Interface list
      RequiredSuperClass: 'SuperClass option
      Variance: GenericVariance }

    override this.ToString() = string this.Name

[<StructuralComparison>]
[<StructuralEquality>]
type Generic<'GenericArg, 'Interface, 'SuperClass> = // TODO: Something being either a type param or type arg doesn't make sense, maybe remove this type?
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
