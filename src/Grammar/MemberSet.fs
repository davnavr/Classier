namespace Classier.NET.Compiler.Grammar

open System.Collections.Immutable

type MemberSet = ImmutableSortedSet<Access * MemberDef>

module MemberSet =
    let comparer =
        { new System.Collections.Generic.IComparer<Access * MemberDef> with
            member _.Compare((_, m1), (_, m2)) =
                invalidOp "bad" }

    let empty = ImmutableSortedSet.Empty.WithComparer comparer
