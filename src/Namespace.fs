namespace Classier.NET.Compiler

open Classier.NET.Compiler.Identifier

type Namespace = // TODO: Move this type to GlobalsTable file?
    | Namespace of IdentifierStr list

    override this.ToString() =
        let (Namespace ns) = this
        match ns with
        | [] -> "<global namespace>"
        | _ ->
            Seq.map string ns
            |> String.concat "."
