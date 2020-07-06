namespace Classier.NET.Compiler.ToSource

open Classier.NET.Compiler

type NamedType<'GenType> =
    | DefinedType of AccessControl.Access * 'GenType
    | ExternType of Extern.EType

type ResolvedType<'GenType> = TypeSystem.Type<NamedType<'GenType>>
