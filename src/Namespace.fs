namespace Classier.NET.Compiler

open Classier.NET.Compiler.Identifier

[<StructuralComparison; StructuralEquality>]
type Namespace =
    | Namespace of IdentifierStr list

    override this.ToString() =
        let (Namespace ns) = this
        match ns with
        | [] -> "<global namespace>"
        | _ ->
            Seq.map string ns
            |> String.concat "."

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Namespace =
    let (|Global|_|) (Namespace ns) =
        match ns with
        | [] -> Some()
        | _ -> None
