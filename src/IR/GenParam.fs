namespace Classier.NET.Compiler.IR

open Classier.NET.Compiler.Grammar

module GenParam =
    [<System.Obsolete>]
    let tupleTypes ptuple =
        ptuple
        |> Seq.map (fun p -> p.Type)
        |> List.ofSeq

    let ofExpParam (eparam: Ast.ExpParam) tresolver =
        { Name = eparam.Name
          Syntax = Param.asInferred eparam
          Type = tresolver eparam.Type }
