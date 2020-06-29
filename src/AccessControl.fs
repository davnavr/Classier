module Classier.NET.Compiler.AccessControl

[<StructuralEquality>]
[<StructuralComparison>]
type GlobalAccess =
    | GlobalPublic
    | GlobalInternal

[<StructuralEquality>]
[<StructuralComparison>]
type Access =
    | Public
    | Internal
    | Protected
    | Private
