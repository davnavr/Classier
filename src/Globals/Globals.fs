namespace Classier.NET.Compiler.Globals

open System
open Classier.NET.Compiler.Identifier

type Namespace =
    | Namespace of IdentifierStr
    | GlobalNamespace

    override this.ToString() =
        match this with
        | Namespace ns -> string ns
        | GlobalNamespace -> String.Empty

type GlobalAccess =
    | GlobalPublic
    | GlobalInternal

[<StructuralEquality>]
[<NoComparison>]
type GlobalType<'DType> =
    | DefinedType of GlobalAccess * 'DType
    | ExternType of EType
