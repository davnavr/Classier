[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Classier.NET.Compiler.Grammar.Decl

let name tdef =
    match tdef with
    | Class cdef -> cdef.ClassName
    | Interface idef -> idef.InterfaceName
    | Module mdef -> Name.asGeneric mdef.ModuleName
