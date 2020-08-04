module Classier.NET.Compiler.Grammar.Param

let create ptype name =
    { Name = name
      Type = ptype }

let asInferred param =
    { Name = param.Name
      Type = Some param.Type }

let name param =
    param.Name
    |> Option.map string
    |> Option.defaultValue "_"

let private strHelper str ps =
    ps
    |> Seq.map str
    |> String.concat ", "
    |> sprintf "(%s)"

let toExpStr: (seq<ExpParam> -> _) =
    (fun param ->
        sprintf "%s: %O" (name param) param.Type)
    |> strHelper

let toInfStr: (seq<InfParam> -> _) =
    (fun param ->
        match param.Type with
        | Some ptype ->
            sprintf "%O: %O" param.Name ptype
        | None -> string param.Name)
    |> strHelper
