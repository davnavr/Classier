module Classier.NET.Compiler.Identifier

open System
open System.Text.RegularExpressions

[<StructuralComparison; StructuralEquality>]
type IdentifierStr =
    | IdentifierStr of string

    override this.ToString() =
        let (IdentifierStr str) = this
        str

[<StructuralComparison; StructuralEquality>]
type Identifier<'Generic> =
    { Name: IdentifierStr
      Generics: 'Generic list }

    override this.ToString() =
        let name = string this.Name
        match this.Generics with
        | [] -> name
        | _ ->
            String.Join(", ", this.Generics)
            |> sprintf "%s<%s>" name

[<StructuralComparison; StructuralEquality>]
type FullIdentifier<'Generic> =
    | FullIdentifier of Identifier<'Generic> * Identifier<'Generic> list

    override this.ToString() =
        let (FullIdentifier(head, ids)) = this
        match ids with
        | [] -> string head
        | _ ->
            String.Join('.', ids)
            |> sprintf "%O.%s" head

let private nameRegex = "^[A-Za-z][A-Za-z_0-9]*$" |> Regex

let defaultSelfId = IdentifierStr "this"

let create str =
    match str with
    | Regex.Matches nameRegex ->
        str
        |> IdentifierStr
        |> Some
    | _ -> None

let ofStr str =
    { Name = str
      Generics = List.empty }

let ofStrSeq strs =
    Seq.tryHead strs
    |> Option.map
        (fun head ->
            let rest =
                strs
                |> Seq.map ofStr
                |> Seq.toList
            FullIdentifier(ofStr head, rest))

let mapGenerics gmapper id =
    { Name = id.Name
      Generics =
          List.map gmapper id.Generics }

let noGenerics<'Generic> = mapGenerics (fun (_: 'Generic) -> ())

let fullAsList (FullIdentifier (head, tail)) =
    head :: tail
