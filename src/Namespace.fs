namespace Classier.NET.Compiler

open Classier.NET.Compiler.Identifier

type Namespace =
    | Namespace of IdentifierStr list

    override this.ToString() =
        let (Namespace ns) = this
        ns
        |> Seq.map string
        |> String.concat "."
