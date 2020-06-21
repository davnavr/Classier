namespace Classier.NET.Compiler.Grammar

open Classier.NET.Compiler.Identifier

type Param<'Type> =
    { Name: IdentifierStr option
      Type: 'Type }

    static member Create ptype name =
        { Name = name
          Type = ptype }

    override this.ToString() =
        let name =
            match this.Name with
            | None -> "_"
            | Some pname -> string pname
        this.Type.ToString()
        |> sprintf "%s%s" name

/// A parameter whose type can be inferred.
type InfParam = Param<TypeName option>
type ExpParam = Param<TypeName>

module Param =
    let create ptype name =
        { Name = name
          Type = ptype }
