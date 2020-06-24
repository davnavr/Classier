module Classier.NET.Compiler.Program

open System
open System.IO

/// The entry point of the compiler.
[<EntryPoint>]
let main args = // TODO: Handle arguments correctly. Need to have an argument that specifies the output.
    let parse =
        args
        |> Array.takeWhile (fun arg -> arg.StartsWith "--" |> not)
        |> Seq.map Path.GetFullPath
        |> Parser.parseFiles System.Text.Encoding.UTF8

    match parse() with
    | Result.Ok (cunits, epoint) ->
        use fileout = new StreamWriter(File.OpenWrite (Array.last args))
        let output =
            Output.write
                { EntryPoint = epoint }
                fileout.WriteLine
        output()
    | Result.Error err -> Console.Error.Write err
    0
