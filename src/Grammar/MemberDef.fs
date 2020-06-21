namespace Classier.NET.Compiler.Grammar

type MemberDef =
    | Member of ConcreteMember
    | Type of TypeDef<MemberDef>
