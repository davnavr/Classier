namespace Classier.NET.Compiler.Globals

open System
open Classier.NET.Compiler.AccessControl
open Classier.NET.Compiler.Identifier

type Namespace =
    | Namespace of IdentifierStr
    | GlobalNamespace

    override this.ToString() =
        match this with
        | Namespace ns -> string ns
        | GlobalNamespace -> String.Empty

module Namespace =
    let ofOpt str =
        match str with
        | Some id -> Namespace id
        | None -> GlobalNamespace

[<StructuralEquality>]
[<NoComparison>]
type GlobalType<'DType> =
    | DefinedType of GlobalAccess * 'DType
    | ExternType of EType
