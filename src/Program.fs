module Classier.NET.Compiler.Program

open System
open System.IO

open Classier.NET.Compiler
open Classier.NET.Compiler.SemAnalysis

/// The entry point of the compiler.
[<EntryPoint>]
let main args = // TODO: Handle arguments correctly. Need to have an argument that specifies the output file.
    let parse =
        args
        |> Array.takeWhile (fun arg -> arg.StartsWith "--" |> not)
        |> Seq.map Path.GetFullPath
        |> Parser.parseFiles System.Text.Encoding.UTF8
    match parse() with
    | Result.Ok (cunits, epoint) ->
        let analysis = Analyze.output (cunits, epoint) GlobalsTable.empty // TODO: How to allow additions of extern types to the globals table?
        match analysis with
        | Result.Ok output ->
            use fileout = new StreamWriter(File.OpenWrite (Array.last args))
            Print.it
                fileout.WriteLine
                (Print.poutput output 4)
            0
        | Result.Error errors ->
            for err in errors do
                err
                |> AnalyzerError.print
                |> stderr.WriteLine
            -1
    | Result.Error err ->
        stderr.WriteLine err
        -1
