namespace Classier.NET.Compiler.ToSource

open Classier.NET.Compiler

type TypeRef<'GenType> =
    | RDefined of 'GenType
    | RExtern of Globals.EType // TODO: Make TypeRef replace GlobalType.
    | RFunc of TypeRef<'GenType> * TypeRef<'GenType>
    | RPrimitive of TypeSystem.PrimitiveType
    | RTuple of TypeRef<'GenType> list
