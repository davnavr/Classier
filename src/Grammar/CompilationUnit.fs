namespace Classier.NET.Compiler.Grammar

type CompilationUnit =
    { Namespace: FullIdentifier option // TODO: Should be IdentifierStr
      Usings: FullIdentifier list
      Source: string
      Types: (Access * TypeDef) list }
