namespace Classier.NET.Compiler

open System.Collections.Immutable
open Classier.NET.Compiler.Grammar

type ExternType =
    { Kind: ExternTypeKind
      Members: ImmutableSortedSet<ExternMember>
      TypeFlags: Flags
      TypeName: Identifier }
and [<RequireQualifiedAccess>]
    ExternTypeKind =
    | Class of ExternType option
    | Interface
    | Module
and [<RequireQualifiedAccess>]
    ExternMember =
    | Constructor of ImmutableArray<Param>
    | Field of Identifier * TypeName
    | Function of
        {| Name: Identifier
           Parameters: ImmutableArray<Param>
           ReturnType: TypeName |}
    | NestedType of ExternType
