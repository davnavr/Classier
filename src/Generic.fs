module Classier.NET.Compiler.Generic

open Classier.NET.Compiler.Identifier
open Classier.NET.Compiler.TypeSystem

[<StructuredFormatDisplay("{Item}")>]
[<StructuralComparison>]
[<StructuralEquality>]
type Generic = // TODO: Remove this union, make GenericArg a union that is either a TypeName<Generic> or a GenericParam.
    | GenericArg of TypeName<Generic>
    | GenericParam of GenericParam // TODO: GenericParam should just be its own type.

    override this.ToString() =
        match this with
        | GenericArg arg -> string arg
        | GenericParam gparam -> string gparam
and GenericVariance =
    | NoVariance
    | Covariant
    | Contravariant
and GenericParam =
    { Name: IdentifierStr
      RequiredInterfaces: FullIdentifier<Generic> list
      RequiredSuperClass: FullIdentifier<Generic> option
      Variance: GenericVariance }

    override this.ToString() = string this.Name

let gparam str =
    { Name = str
      RequiredInterfaces = List.empty
      RequiredSuperClass = None
      Variance = NoVariance }
    |> GenericParam
