namespace Classier.NET.Compiler

open System

type TypeName =
    | FuncType of
        {| ParamType: TypeName
           ReturnType: TypeName |}
    | Identifier of Identifier list
    | Inferred
    | Tuple of TypeName list
    | Union of TypeName list

    override this.ToString() =
        match this with
        | FuncType f -> sprintf "%A => %A" (f.ParamType) (f.ReturnType)
        | Identifier names -> String.Join('.', names)
        | Inferred -> "_"
        | Tuple types -> sprintf "(%s)" (String.Join(", ", types))
        | Union options -> String.Join(" | ", options)
and Identifier =
    { Name: string
      GenericArgs: GenericArg list }

    override this.ToString() =
        match this.GenericArgs with
        | [] -> this.Name
        | _ ->
            let gargs = String.Join(", ", this.GenericArgs)
            sprintf "%s<%s>" this.Name gargs
and GenericArg = TypeName

module Identifier =
    let ofStrings names =
        names
        |> List.map (fun name ->
            { Name = name
              GenericArgs = [] })
