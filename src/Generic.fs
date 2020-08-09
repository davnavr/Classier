module Classier.NET.Compiler.Generic

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

let param str =
    { Name = str
      RequiredInterfaces = List.empty
      RequiredSuperClass = None
      Variance = NoVariance }
