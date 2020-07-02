namespace Classier.NET.Compiler

open Classier.NET.Compiler.Identifier

type Namespace =
    | Namespace of IdentifierStr list

    override this.ToString() =
        let (Namespace ns) = this
        match ns with
        | [] -> "<global namespace>"
        | _ ->
            Seq.map string ns
            |> String.concat "."
