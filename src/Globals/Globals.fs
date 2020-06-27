namespace Classier.NET.Compiler.Globals

open System
open Classier.NET.Compiler
open Classier.NET.Compiler.Identifier

type Namespace =
    | Namespace of IdentifierStr
    | GlobalNamespace

    override this.ToString() =
        match this with
        | Namespace ns -> string ns
        | GlobalNamespace -> String.Empty

[<StructuralEquality>]
[<NoComparison>]
type GlobalType<'DType> =
    | DefinedType of 'DType
    | ExternType of EType

type GlobalTypeSymbol =
    { Namespace: Namespace
      Type: GlobalType<Grammar.TypeDef> }
