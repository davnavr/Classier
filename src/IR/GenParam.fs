namespace Classier.NET.Compiler.IR

module GenParam =
    [<System.Obsolete>]
    let tupleTypes ptuple =
        ptuple
        |> Seq.map (fun p -> p.Type)
        |> List.ofSeq
