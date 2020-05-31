module Classier.NET.Compiler.Program

open System

/// The entry point of the compiler.
[<EntryPoint>]
let main args =
    printf "%s" (FParsec.CharParsers.pfloat.GetType().Assembly.Location)
    -1
