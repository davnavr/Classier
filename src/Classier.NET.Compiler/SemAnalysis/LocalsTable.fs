namespace Classier.NET.Compiler.SemAnalysis

open System.Collections.Immutable
open Classier.NET.Compiler.Grammar

type Local =
    { Name: string
      Type: TypeName }

type LocalsTable = LocalsTable of ImmutableList<Local> list

module LocalsTable =
    let empty = LocalsTable List.empty

    let enterScope (LocalsTable table) = ImmutableList.Empty :: table |> LocalsTable
