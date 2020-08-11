module Classier.NET.Compiler.Program

open System
open System.IO

open Classier.NET.Compiler
open Classier.NET.Compiler.SemAnalysis

module TempStandardLib =
    open Classier.NET.Compiler.Extern

    let table gtable =
        let system =
            Namespace [ IdentifierStr.create "System" ]
        let console =
            { EModule.ModuleName =
                Identifier.create "Console"
              Members =
                { FunctionName =
                    Identifier.create "WriteLine"
                  Parameters =
                    { EParam.Name =
                        IdentifierStr.create "value"
                      Type =
                        TypeSystem.Primitive TypeSystem.PrimitiveType.Unit }
                    |> ImmArray.singleton
                    |> ImmArray.singleton
                  ReturnType =
                    TypeSystem.Primitive TypeSystem.PrimitiveType.Unit }
                |> EFunction
                |> TypeOrMember.Member
                |> SortedSet.singleton }
            |> EGlobalModule
            |> Extern
        Globals.addType
            console
            system
            gtable
        |> Result.get

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
        let analysis =
            Analyze.output
                (cunits, epoint)
                (TempStandardLib.table Globals.emptyTable) // TODO: How to allow additions of extern types to the globals table?
        match analysis with
        | Result.Ok output ->
            use fileout = new StreamWriter(File.OpenWrite (Array.last args))
            Print.it
                fileout.Write
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
