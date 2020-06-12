module Classier.NET.Compiler.Generic

open Classier.NET.Compiler.Identifier
open Classier.NET.Compiler.TypeSystem

[<StructuredFormatDisplay("{Item}")>]
[<StructuralComparison>]
[<StructuralEquality>]
type Generic =
    | GenericArg of TypeName<Generic>
    | GenericParam of GenericParam

    override this.ToString() = string this
and GenericVariance =
    | NoVariance
    | Covariant
    | Contravariant
and [<StructuredFormatDisplay("{Name}")>]
    GenericParam =
    { Name: string
      RequiredSuperClass: Identifier<Generic> list
      RequiredInterfaces: TypeName<Generic> list
      Variance: GenericVariance }
