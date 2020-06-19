module Classier.NET.Compiler.Generic

open Classier.NET.Compiler.Identifier
open Classier.NET.Compiler.TypeSystem

[<StructuredFormatDisplay("{Item}")>]
[<StructuralComparison>]
[<StructuralEquality>]
type Generic =
    | GenericArg of TypeName<Generic>
    | GenericParam of GenericParam

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
      RequiredSuperClass: OptIdentifier<Generic>
      Variance: GenericVariance }

    override this.ToString() = string this.Name

let gparam str =
    { Name = str
      RequiredInterfaces = List.empty
      RequiredSuperClass = OptIdentifier.EmptyIdentifier
      Variance = NoVariance }
    |> GenericParam

let garg strs =
    (invalidOp "Neet to get a full name here")
    |> TypeName.Identifier
    |> GenericArg
