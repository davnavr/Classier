namespace Classier.NET.Compiler.Grammar

open Classier.NET.Compiler
open Classier.NET.Compiler.AccessControl
open Classier.NET.Compiler.Identifier

type CompilationUnit =
    { Namespace: Namespace
      Usings: FullIdentifier<Generic.Generic> list
      Source: string
      Types: (GlobalAccess * TypeDef) list }
