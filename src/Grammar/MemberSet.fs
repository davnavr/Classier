namespace Classier.NET.Compiler.Grammar

open System.Collections.Generic
open System.Collections.Immutable

module MemberSet =
    let private emptySet f =
        ImmutableSortedSet.Empty.WithComparer
            { new IComparer<_> with member _.Compare(m1, m2) = f m1 m2 }

    let classSet =
        emptySet
            (fun m1 m2 ->
                match (m1, m2) with
                | (Member i1, Member i2) ->
                    let paramCompare =
                        compare
                            (Member.instanceParams i1)
                            (Member.instanceParams i2)
                    match paramCompare with
                    | 0 ->
                        compare
                            (Member.instanceName i1)
                            (Member.instanceName i2)
                    | _ -> paramCompare
                | (Member mdef, Type tdef)
                | (Type tdef, Member mdef) ->
                    let factor =
                        match m1 with
                        | Member _ -> 1
                        | Type _ -> -1
                    let nameCompare =
                        mdef
                        |> Member.instanceName
                        |> compare (Some tdef.ClassName)
                    nameCompare * factor
                | (Type t1, Type t2) ->
                    compare t1.ClassName t2.ClassName)
