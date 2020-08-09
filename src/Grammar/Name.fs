module Classier.NET.Compiler.Grammar.Name

open Classier.NET.Compiler

let simple pos (str: IdentifierStr) =
    { Identifier = str
      Position = pos }

let ofStr pos str =
    { Identifier = Identifier.ofStr str
      Position = pos }

let asGeneric name = ofStr name.Position name.Identifier
