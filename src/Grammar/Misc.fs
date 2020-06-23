namespace Classier.NET.Compiler.Grammar

type CompilationUnit =
    { EntryPoint: EntryPoint option
      Namespace: FullIdentifier option
      Usings: FullIdentifier list
      Source: string
      Types: MemberList<TypeDef> }
