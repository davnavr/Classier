module Classier.NET.Compiler.LocalsTable

open System.Collections.Immutable
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.IR
open Classier.NET.Compiler.Identifier

type Local =
    { Name: IdentifierStr
      Type: ResolvedType }

type Error =
    | DuplicateLocal of Local
    | EmptyTable

type LocalsTable =
    private
    | LocalsTable of ImmutableSortedSet<Local> list

let private emptyScope =
    SortedSet.withComparer
        (fun one two ->
            compare one.Name two.Name)
        ImmutableSortedSet.Empty

let empty = LocalsTable List.empty

let enterScope (LocalsTable table) = emptyScope :: table |> LocalsTable
let exitScope (LocalsTable table) =
    match table with
    | [] -> None
    | _ -> LocalsTable table.Tail |> Some

// TODO: How will type resolution work here?
let addLocal lname ltype (LocalsTable table) =
    match table with
    | [] -> Result.Error EmptyTable
    | scope :: rest ->
        result {
            let! added =
                SortedSet.tryAdd
                    { Name = lname
                      Type = ltype }
                    (List.head table)
                |> Result.mapError DuplicateLocal
            return scope :: (List.tail table) |> LocalsTable
        }

let addExpParam (eparam: ExpParam) rtype table =
    match eparam.Name with
    | Some pname ->
        addLocal
            pname
            (rtype eparam.Type)
            table
    | None -> Result.Ok table
