module Classier.NET.Compiler.LocalsTable

open System.Collections.Generic
open System.Collections.Immutable
open Classier.NET.Compiler.Grammar

[<NoComparison>]
[<CustomEquality>]
type Local =
    { Name: string
      Type: TypeName }

    override this.Equals obj =
        match obj with
        | :? Local as other ->
            this.Name = other.Name
        | _ -> false

    override this.GetHashCode() = this.Name.GetHashCode()
    
type LocalsTable = LocalsTable of ImmutableSortedSet<Local> list

let private emptyScope =
    let localComparer =
        { new IComparer<Local> with
              member _.Compare(l1, l2) =
                  l1.Name.CompareTo(l2.Name) }
    ImmutableSortedSet.Empty.WithComparer(localComparer)

let empty = LocalsTable List.empty

let enterScope (LocalsTable table) = emptyScope :: table |> LocalsTable
let exitScope (LocalsTable table) =
    match table with
    | [] -> List.empty
    | _ -> table.Tail
    |> LocalsTable

let addLocal local (LocalsTable table) =
    match table with
    | [] -> table
    | _ ->
        // TODO: Maybe return None if it already exists in the scope?
        table.Head.Add(local) :: table.Tail
    |> LocalsTable
