module Classier.NET.Compiler.LocalsTable

open System.Collections.Generic
open System.Collections.Immutable
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.Identifier

[<NoComparison>]
[<CustomEquality>]
type Variable =
    { Name: IdentifierStr
      Type: TypeName }

    override this.Equals obj =
        match obj with
        | :? Variable as other ->
            this.Name = other.Name
        | _ -> false

    override this.GetHashCode() = this.Name.GetHashCode()

type Local =
    | Local of Variable

type LocalsTable = LocalsTable of ImmutableSortedSet<Local> list

let private emptyScope =
    { new IComparer<Local> with
          member _.Compare(l1, l2) =
            match (l1, l2) with
            | (Local one, Local two) ->
                compare one.Name two.Name }
    |> ImmutableSortedSet.Empty.WithComparer

let empty = LocalsTable List.empty

let enterScope (LocalsTable table) = emptyScope :: table |> LocalsTable
let exitScope (LocalsTable table) =
    match table with
    | [] -> None
    | _ -> LocalsTable table.Tail |> Some

let addLocal local (LocalsTable table) =
    table
    |> List.tryHead
    |> Option.map
        (fun locals ->
            locals
            |> SortedSet.tryAdd local
            |> Option.map
                (fun newSet ->
                    (newSet :: table.Tail)
                    |> LocalsTable))
    |> Option.flatten
