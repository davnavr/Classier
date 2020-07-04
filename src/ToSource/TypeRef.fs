namespace Classier.NET.Compiler.ToSource

open Classier.NET.Compiler

type NamedType<'GenType> =
    | DefinedType of AccessControl.Access * 'GenType
    | ExternType of Extern.EType

type TypeRef<'GenType> = TypeSystem.TypeName<NamedType<'GenType>>
