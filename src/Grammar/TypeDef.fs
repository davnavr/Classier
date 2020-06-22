[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Classier.NET.Compiler.Grammar.TypeDef

let name tdef =
    match tdef with
    | Class cdef -> cdef.ClassName
    | Interface idef -> idef.InterfaceName
    | Module mdef -> Name.asGeneric mdef.ModuleName

let compare t1 t2 =
    let nameCompare =
        compare
            (name t1)
            (name t2)
    match (nameCompare, t1, t2) with
    | (0, Module _, Module _) -> 0
    | (0, Module _, _)
    | (0, _ , Module _) ->
        match t1 with
        | Module _ -> 1
        | _ -> -1
    | _ -> nameCompare

let getMembers tdef =
    let map mtype members =
        Seq.map (snd >> mtype) members
    match tdef with
    | Class cdef -> // NOTE: cdef.Members was empty for some reason!
        map ClassMember cdef.Members
    | Interface idef ->
        map InterfaceMember idef.Members
    | Module mdef ->
        map ModuleMember mdef.Members
