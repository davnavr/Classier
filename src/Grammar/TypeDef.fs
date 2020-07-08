[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Classier.NET.Compiler.Grammar.TypeDef

let name tdef =
    match tdef with
    | Class cdef -> cdef.ClassName
    | Interface idef -> idef.InterfaceName
    | Module mdef -> Name.asGeneric mdef.ModuleName
