module Classier.NET.Compiler.Grammar.Expression

open Classier.NET.Compiler.Identifier

let withPos pos expr = pos, expr

let emptyCase =
    { Body = List.empty
      Patterns = [ Pattern.Default ] }
