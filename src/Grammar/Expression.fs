module Classier.NET.Compiler.Grammar.Expression

let withPos pos expr = pos, expr

let emptyCase =
    { Body = List.empty
      Patterns = [ Pattern.Default ] }
