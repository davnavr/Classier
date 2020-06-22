[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Classier.NET.Compiler.Grammar.TypeDef

let name tdef =
    match tdef with
    | Class cdef -> cdef.ClassName
    | Interface idef -> idef.InterfaceName
    | Module mdef -> Name.asGeneric mdef.ModuleName

let getMembers tdef =
    let map mtype members =
        Seq.map (snd >> mtype) members
    match tdef with
    | Class cdef ->
        map ClassMember cdef.Members
    | Interface idef ->
        map InterfaceMember idef.Members
    | Module mdef ->
        map ModuleMember mdef.Members
