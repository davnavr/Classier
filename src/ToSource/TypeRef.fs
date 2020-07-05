namespace Classier.NET.Compiler.ToSource

open Classier.NET.Compiler

type NamedType<'GenType> =
    | DefinedType of AccessControl.Access * 'GenType
    | ExternType of Extern.EType

type TypeRef<'GenType> = TypeSystem.Type<NamedType<'GenType>> // TODO: Rename to ResolvedType.
