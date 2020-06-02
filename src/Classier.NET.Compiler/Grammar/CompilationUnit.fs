namespace Classier.NET.Compiler.Grammar

type CompilationUnit =
    { TypeDefs: TypeDef list
      Namespace: string list
      Usings: Identifier list list }
