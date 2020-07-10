module Classier.NET.Compiler.AccessControl

[<StructuralComparison; StructuralEquality>]
type GlobalAccess =
    | GlobalPublic
    | GlobalInternal

[<StructuralComparison; StructuralEquality>]
type Access =
    | Public
    | Internal
    | Protected
    | Private
