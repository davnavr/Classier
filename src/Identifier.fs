module Classier.NET.Compiler.Identifier

open System
open System.Text.RegularExpressions

[<StructuralComparison>]
[<StructuralEquality>]
type Identifier<'Generic> =
    { Name: string
      Generics: 'Generic list }

    override this.ToString() =
        match this.Generics with
        | [] -> this.Name
        | _ ->
            String.Join(", ", this.Generics)
            |> sprintf "%s<%s>" this.Name

[<StructuralComparison>]
[<StructuralEquality>]
type FullIdentifier<'Generic> =
    | FullIdentifier of Identifier<'Generic> list // TODO: Make this an Identifier<'Generic> list option

    static member Empty = List.empty<Identifier<'Generic>> |> FullIdentifier

    override this.ToString() =
        let (FullIdentifier ids) = this
        String.Join('.', ids)

let private nameRegex = "^[A-Za-z_][A-Za-z_0-9]*$" |> Regex

let ofString name =
    if nameRegex.IsMatch name 
    then Some { Name = name; Generics = [] }
    else None

let ofStrings<'Generic> names =
    names
    |> Seq.fold
        (fun list name ->
            match list with
            | Some prev ->
                match ofString name with
                | Some (next: Identifier<'Generic>) ->
                    prev @ [ next ] |> Some
                | _ -> None
            | _ -> None)
        (Some List.Empty)
    |> Option.map FullIdentifier
    |> Option.defaultValue FullIdentifier.Empty
