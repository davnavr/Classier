module Classier.NET.Compiler.AccessControl

[<StructuralComparison; StructuralEquality>]
type GlobalAccess =
    | GlobalPublic
    | GlobalInternal

[<RequireQualifiedAccess>]
[<StructuralComparison; StructuralEquality>]
type PublicAccess =
    | Public
    | Protected

[<StructuralComparison; StructuralEquality>]
type Access =
    | Public
    | Internal
    | Protected
    | Private
