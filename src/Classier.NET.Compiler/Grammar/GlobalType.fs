namespace Classier.NET.Compiler.Grammar

open System
open Classier.NET.Compiler

[<StructuralEquality>]
[<NoComparison>]
type GlobalType =
    | DefinedType of TypeDef
    | ExternType of ExternType

module GlobalType =
    let getName gtype =
        match gtype with
        | DefinedType tdef -> tdef.Definition.Identifier
        | ExternType etype -> etype.TypeName

type GlobalTypeSymbol =
    { Namespace: string list
      Type: GlobalType }

    static member ofTypeDef ns typeDef =
        { Namespace = ns
          Type = DefinedType typeDef }
