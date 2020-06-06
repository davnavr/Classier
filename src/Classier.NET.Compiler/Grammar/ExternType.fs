namespace Classier.NET.Compiler.Grammar

open System.Collections.Immutable

type ExternType =
    { Kind: ExternTypeKind
      Members: ImmutableSortedSet<ExternMember>
      TypeFlags: Flags
      TypeName: Identifier }
and [<RequireQualifiedAccess>]
    ExternTypeKind =
    | Class of ExternType
    | Interface
    | Module
and [<RequireQualifiedAccess>]
    ExternMember =
    | NestedType of ExternType
