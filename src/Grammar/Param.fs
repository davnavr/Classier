namespace Classier.NET.Compiler.Grammar

open Classier.NET.Compiler.Identifier

type Param<'Type> =
    { Name: IdentifierStr option
      Type: 'Type }

/// A parameter whose type can be inferred.
type InfParam = Param<TypeName option>
type ExpParam = Param<TypeName>

module Param =
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
            sprintf "%s: %s"
                (name param)
                (string param.Type))
        |> strHelper

    let toInfStr: (seq<InfParam> -> _) =
        (fun param ->
            match param.Type with
            | Some ptype ->
                sprintf
                    "%s: %s"
                    (string param.Name)
                    (string ptype)
            | None -> string param.Name)
        |> strHelper
