namespace Classier.NET.Compiler.Grammar

open Classier.NET.Compiler.AccessControl
open Classier.NET.Compiler.Identifier

type CompilationUnit =
    { Namespace: IdentifierStr option
      Usings: FullIdentifier list
      Source: string
      Types: (GlobalAccess * TypeDef) list }
