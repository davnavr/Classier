module Classier.NET.Compiler.Generic

open Classier.NET.Compiler.Identifier
open Classier.NET.Compiler.TypeSystem

type Generic =
    | GenericArg of TypeName<Generic>
    | GenericParam of GenericParam

    override this.ToString() =
        match this with
        | GenericArg name -> name.ToString()
        | GenericParam param -> param.ToString()
and GenericVariance =
    | NoVariance
    | Covariant
    | Contravariant
and GenericParam =
    { Name: string
      RequiredSuperClass: Identifier<Generic> list
      RequiredInterfaces: TypeName<Generic> list
      Variance: GenericVariance }

    override this.ToString() = this.Name
