namespace Classier.NET.Compiler.Grammar

[<RequireQualifiedAccess>]
type GlobalType =
    | DefinedType of TypeDef
    | ExternType of ExternType

    static member getName gtype =
        match gtype with
        | GlobalType.DefinedType tdef -> tdef.Definition.Identifier.ToString()
        | _ -> invalidOp "not implemented"

type GlobalTypeSymbol =
    { Namespace: string list
      Type: GlobalType }

    static member ofTypeDef ns typeDef =
        { Namespace = ns
          Type = GlobalType.DefinedType typeDef }
