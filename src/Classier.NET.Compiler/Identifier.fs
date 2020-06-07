module Classier.NET.Compiler.Identifier

open System

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

let ofString name = { Name = name; Generics = [] }

let ofStrings names =
    names
    |> List.map (fun name ->
        { Name = name
          Generics = [] })
