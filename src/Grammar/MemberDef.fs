namespace Classier.NET.Compiler.Grammar

type MemberDef =
    | ClassMember of ClassMember
    | InterfaceMember of InterfaceMember
    | ModuleMember of TypeOrMember<TypeDef, StaticMember>
    | GlobalType of TypeDef

module MemberDef =
    let fold classm intfm modlm typem mdef =
        match mdef with
        | ClassMember cmem -> classm cmem
        | InterfaceMember imem -> intfm imem
        | ModuleMember mmem -> modlm mmem
        | GlobalType tdef -> typem tdef
