[<RequireQualifiedAccess>]
module Classier.NET.Compiler.SemAnalysis.Analyze

open Classier.NET.Compiler

open Classier.NET.Compiler.IR

let output (cunits, epoint) =
    let result = init Globals.emptyTable
    match result.Errors with
    | ImmList.Empty ->
        { GlobalDecls =
            Seq.map fst result.GlobalDecls
          EntryPoint = result.EntryPoint }
        |> Result.Ok
    | err -> Result.Error err
