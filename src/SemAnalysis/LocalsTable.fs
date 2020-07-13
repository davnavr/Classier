﻿module Classier.NET.Compiler.LocalsTable

open System.Collections.Immutable
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.IR
open Classier.NET.Compiler.Identifier

type private Local =
    { Name: IdentifierStr
      Type: ResolvedType }

type LocalsTable =
    private
    | LocalsTable of ImmutableSortedSet<Local> list

let private emptyScope =
    SortedSet.withComparison
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
    table
    |> List.tryHead
    |> Option.map
        (fun locals ->
            locals
            |> SortedSet.tryAdd
                { Name = lname
                  Type = ltype }
            |> Option.map
                (fun newSet ->
                    (newSet :: table.Tail)
                    |> LocalsTable))
    |> Option.flatten

let addExpParam (eparam: ExpParam) rtype table =
    match eparam.Name with
    | Some pname ->
        addLocal
            pname
            (rtype eparam.Type)
            table
    | None -> Some table
