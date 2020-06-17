﻿module Classier.NET.Compiler.Extern

open System.Collections.Immutable
open Classier.NET.Compiler.Grammar

type ExternType =
    { Kind: ExternTypeKind
      Members: ImmutableSortedSet<ExternMember>
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