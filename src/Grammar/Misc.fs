namespace Classier.NET.Compiler.Grammar

type EntryPoint =
    { Body: Statement list
      Origin: Position
      Parameters: ExpParam list }

type CompilationUnit =
    { EntryPoint: EntryPoint option
      Namespace: FullIdentifier option
      Usings: FullIdentifier list
      Source: string
      Types: seq<Access * TypeDef> }
