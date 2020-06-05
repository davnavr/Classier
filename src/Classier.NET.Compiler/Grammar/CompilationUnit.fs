namespace Classier.NET.Compiler.Grammar

type CompilationUnit =
    { Definitions: TypeDef list
      Namespace: string list
      Usings: Identifier list list }
