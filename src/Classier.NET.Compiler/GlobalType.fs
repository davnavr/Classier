module Classier.NET.Compiler.GlobalType

open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.Extern

[<StructuralEquality>]
[<NoComparison>]
type GlobalType =
    | DefinedType of TypeDef
    | ExternType of ExternType

type GlobalTypeSymbol =
    { Namespace: string list
      Type: GlobalType }
    
    static member ofTypeDef ns typeDef =
        { Namespace = ns
          Type = DefinedType typeDef }

let getName gtype =
    match gtype with
    | DefinedType tdef -> tdef.Definition.Identifier
    | ExternType etype -> etype.TypeName
